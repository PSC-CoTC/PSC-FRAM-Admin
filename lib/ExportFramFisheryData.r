################
#
# Code to export a file that can be used as a template to update a particular person's catch data.
#
# Nicholas Komick
# nicholas.komick@dfo-mpo.gc.ca
# February 9, 2016
# Using: http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html
#
################

rm(list=ls())   		#clean up the workspace
header <- "Export FRAM Fishery Data v0.1"

# Column names: Fishery ID, Fishery Name, Time Step ID, Flag ID, Non-Selective Catch, MSF Catch, CNR Mortality


source.lib.dir <- "./lib/"
if (exists("lib.dir")) {
  source.lib.dir <- lib.dir
} 


if (exists("report.dir") == FALSE) {
  report.dir <- "./report/"
  dir.create(report.dir, showWarnings = FALSE)
}

source(file.path(source.lib.dir, "Util.r"))
source(file.path(source.lib.dir, "FramDb.r"))

required.packages <- c("RODBC", "dplyr")
InstallRequiredPackages(required.packages)

config.file.name <- NA
cmdArgs <- commandArgs(TRUE)
if(length(cmdArgs) > 0) {
  print(cmdArgs)
  config.file.name <- cmdArgs[1]
  cat(sprintf("Using configuration file '%s'\n", config.file.name))
} else {
  config.file.name <- "./config/export_fram_fishery_config.r"
  cat(sprintf("WARNING - configuration file not provided, default file '%s' is used.\n", config.file.name))
}

LoadConfigFiles(report.config.file=config.file.name)

cat(header)
cat("\n\n")
cat(sprintf("Using db file: %s\n", fram.db.name))
cat(sprintf("Using run name: %s\n", fram.run.name))
cat("\n")

fram.db.conn <- odbcConnectAccess(normalizePath(fram.db.name))
#fram.db.conn <- odbcDriverConnect(sprintf('DRIVER={Microsoft Access Driver (*.mdb)};DBQ="%s"', fram.db.name))


fishery.mortality <- GetFisheryMortality(fram.db.conn, fram.run.name, run.year)
fisheries <- GetFramFisheries(fram.db.conn)
fishery.mortality <- inner_join (fisheries, fishery.mortality, by=c("fishery.id"))
by.stock <- group_by(fishery.mortality, stock.id)
stock.mort <- summarise(by.stock, total.fishery.mortality = sum(fishery.mortality, na.rm=TRUE))

escapement <- GetTotalEscapement (fram.db.conn, fram.run.name, run.year)
escapement <- left_join(escapement, stock.mort, by=c("stock.id"))
fram.stocks <- GetFramStocks(fram.db.conn)
escapement <- left_join(escapement, fram.stocks, by=c("stock.id"))

fishery.mortality <- left_join(fishery.mortality, escapement, by=c("run.id", "run.year", "stock.id"))


fishery.mortality <- mutate(fishery.mortality, cohort.age.3 = total.fishery.mortality + escapement)
fishery.mortality <- mutate(fishery.mortality, fishery.er = fishery.mortality / cohort.age.3)

export.file.name <- sprintf("%sFishery_Mortalities_%s_%s_%s.csv", 
                            report.dir,
                            run.year,
                            fram.run.name,
                            GetTimeStampText())

WriteCsv(export.file.name, fishery.mortality)

cat("The FRAM Fishery Export is provided in the file below:\n\n")
cat(export.file.name)
cat("\n\n")