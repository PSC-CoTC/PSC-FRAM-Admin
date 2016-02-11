################
#
# Code to generate annual report tables for the Coho Technical Committee.
#
# Nicholas Komick
# nicholas.komick@dfo-mpo.gc.ca
# January 14, 2015
# Using: http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html
#
################

rm(list=ls()) #clean up the workspace
header <- "CoTC Annual Report Tool v0.1a"

source.lib.dir <- "./lib/"
if (exists("lib.dir")) {
  source.lib.dir <- lib.dir
} 


if (exists("report.dir") == FALSE) {
  report.dir <- "./report/"
}

if (exists("data.dir") == FALSE) {
  data.dir <- "./data/"
}

source(file.path(source.lib.dir, "AnnualReportLib.r"))


required.packages <- c("knitr", "tools")
InstallRequiredPackages(required.packages)

cmdArgs <- commandArgs(TRUE)
if(length(cmdArgs) > 0) {
  print(cmdArgs)
} else {
  cat("No command line parameters provided.\n")
}

config.file.name <- cmdArgs[1]

if (length(cmdArgs) == 0) {
  config.file.name <- "./config/2014_report_config.r"
}

LoadConfigFiles(report.config.file=config.file.name)

cat(header)
cat("\n")

psc.data.list <- LoadPscData(data.dir)

pre.season.db.conn <- odbcConnectAccess(pre.season.fram.db)  
pre.season.data <- CompilePscData(pre.season.db.conn, pre.season.run.name, run.year, psc.data.list)
odbcClose(pre.season.db.conn)

post.season.db.conn <- odbcConnectAccess(post.season.fram.db)  
post.season.data <- CompilePscData(post.season.db.conn, post.season.run.name, run.year, psc.data.list)
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

report.filename <- paste0(report.dir, run.year,"_AnnualReport.html")
knit(paste0(source.lib.dir, "AnnualReport.Rhtml"), output=report.filename)


