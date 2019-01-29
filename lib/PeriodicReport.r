################
#
# Code to generate comparison report tables (post-season original vs. post-season updated)
# for reviewing inputs to the Periodic Reports of the Coho Technical Committee.
# M. Thiess, mary.thiess@dfo-mpo.gc.ca (January 2019)
# Note:  TAMM files are only used for post-season BK runs starting in 2015. 
#
# -->Based on AnnualReport.r by Nicholas Komick (nicholas.komick@dfo-mpo.gc.ca)
# January 14, 2015
# Using: http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html
#
################

rm(list=ls()) #clean up the workspace
header <- "CoTC Periodic Report Tool v0.1"

kAnnualReportBaseName <- "PeriodicReport"

source.lib.dir <- "./lib/"
if (exists("lib.dir")) {
  source.lib.dir <- lib.dir
} 

if (exists("template.dir") == FALSE) {
  template.dir <- "./templates/"
}

if (exists("report.dir") == FALSE) {
  report.dir <- "./report/"
}

if (exists("data.dir") == FALSE) {
  data.dir <- "./data/"
}

source(file.path(source.lib.dir, "AnnualReportLib.r"))
source(file.path(source.lib.dir, "TammData.r"))

required.packages <- c("knitr", "tools")
InstallRequiredPackages(required.packages)

cmdArgs <- commandArgs(TRUE)
if(length(cmdArgs) > 0) {
  print(cmdArgs)
} else {
  cat("No command line parameters provided.\n")
}

config.file.name <- choose.files(caption = "Select report config file", multi=FALSE, filters = Filters[c("txt", "All"),])

LoadConfigFiles(report.config.file=config.file.name)

cat(header)
cat("\n")

if (exists("pre.season.tamm") == FALSE) {
  pre.season.tamm <- NA
} 

if (exists("post.season.tamm") == FALSE) {
  post.season.tamm <- NA
} 

if (run.year>=2015){
if (is.na(pre.season.tamm) || is.na(post.season.tamm)) {
  if ((is.na(pre.season.tamm) && is.na(post.season.tamm)) == FALSE) {
    stop("Both the Pre and Post season tamm spreadsheet must be defined and not just one.")
  }
}
}

if (run.year<=2014){
  pre.season.tamm.name <- "No TAMM used"
  if(is.na(post.season.tamm)){
    stop("Run year prior to 2015: Pre season tamm is NA. Post season tamm must be defined.")
  }
}

psc.data.list <- LoadPscData(data.dir)

pre.season.db.conn <- odbcConnectAccess(pre.season.fram.db)
if (!is.na(pre.season.tamm)) {
  pre.season.tamm <- normalizePath(pre.season.tamm)
  pre.season.tamm.name <- pre.season.tamm
  if (!exists("pre.season.tamm.fishery.ref")) {
    stop("If the preseason tamm is defined, the you must define \"pre.season.tamm.fishery.ref\"")
  }
  if (!exists("pre.season.tamm.esc.ref")) {
    stop("If the preseason tamm is defined, the you must define \"pre.season.tamm.esc.ref\"")
  }
  pre.tamm.list <- GetTammData(pre.season.tamm, 
                               pre.season.tamm.fishery.ref,
                               pre.season.tamm.esc.ref) 
} else {
  pre.tamm.list <- NULL
}
pre.season.data <- CompilePscData(pre.season.db.conn, pre.season.run.name, run.year, psc.data.list, pre.tamm.list)
odbcClose(pre.season.db.conn)

post.season.db.conn <- odbcConnectAccess(post.season.fram.db)
if (!is.na(post.season.tamm)) {
  post.season.tamm <- normalizePath(post.season.tamm)
  if (!exists("post.season.tamm.fishery.ref")) {
    stop("If the post season TAMM is defined, the you must define \"post.season.tamm.fishery.ref\"")
  }
  if (!exists("post.season.tamm.esc.ref")) {
    stop("If the post season TAMM is defined, the you must define \"post.season.tamm.esc.ref\"")
  }
  post.tamm.list <- GetTammData(post.season.tamm, 
                                post.season.tamm.fishery.ref,
                                post.season.tamm.esc.ref)
} else {
  post.tamm.list <- NULL
}

post.season.data <- CompilePscData(post.season.db.conn, post.season.run.name, run.year, psc.data.list, post.tamm.list)
odbcClose(post.season.db.conn)

annual.tbl.third <- CreateTable3(post.season.data)
report.filename <- file.path(report.dir, paste0(run.year, "_annual_table3.csv"))
WriteCsv(report.filename, annual.tbl.third)
cat(sprintf("The annual report table 3 written to:\n\t%s\n\n", normalizePath(report.filename)))

annual.tbl.second <- CreateTable2(pre.season.data, post.season.data, run.year)
report.filename <- file.path(report.dir, paste0(run.year, "_annual_table2.csv"))
WriteCsv(report.filename, annual.tbl.second)
cat(sprintf("The annual report table 2 written to:\n\t%s\n\n", normalizePath(report.filename)))

annual.tbl.first <- CreateTable1(pre.season.data, post.season.data, run.year)
report.filename <- file.path(report.dir, paste0(run.year, "_annual_table1.csv"))
WriteCsv(report.filename, annual.tbl.first)
cat(sprintf("The annual report table 1 written to:\n\t%s\n\n", normalizePath(report.filename)))  

report.filename <- paste0(report.dir, run.year,"_", kAnnualReportBaseName, ".html")
report.template.filename <- paste0(template.dir, run.year, "_", kAnnualReportBaseName, ".rhtml")
if (file.exists(report.template.filename) == FALSE) {
  #No year specific template file, so use default annual report file template.
  report.template.filename <- paste0(template.dir, kAnnualReportBaseName, ".rhtml")
  cat(sprintf("\nWARNING: NO specific annual report template, using default template: %s\n\n", 
              report.template.filename)) 
}
knit(report.template.filename, output=report.filename)


