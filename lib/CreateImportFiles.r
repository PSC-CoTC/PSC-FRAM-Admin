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
header <- "Create Import File Tool v0.2 alpha"

# Column names: Fishery ID, Fishery Name, Time Step ID, Flag ID, Non-Selective Catch, MSF Catch, CNR Mortality


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
  config.file.name <- "./config/create_import_config.r"
  cat(sprintf("WARNING - configuration file not provided, default file '%s' is used.\n", config.file.name))
}

LoadConfigFiles(report.config.file=config.file.name)

cat(header)
cat("\n\nCreating Import Files from")
cat(sprintf("Database file: %s\n", fram.db.name))
cat(sprintf("Run name: %s\n", fram.run.name))
cat("\n")

fram.db.conn <- odbcConnectAccess(fram.db.name)

###### Extract data from FRAM database

fishery.scalars <- GetFramFisheryScalars(fram.db.conn, fram.run.name)
#Drop the FRAM fishery column, this is provided in other data frames
fishery.scalars <- select(fishery.scalars, -one_of("fram.fishery.name"))

backward.esc <- GetFramBackwardEscapement(fram.db.conn, fram.run.name)
base.fishery <- GetFramBaseFisheries(fram.db.conn, fram.run.name)
stocks <- GetFramStocks(fram.db.conn)


odbcClose(fram.db.conn)

###### Compile Fishery Catch Data Frame  #####################
fishery.scalars <- left_join(base.fishery, fishery.scalars, by=c("fram.run.id", "fram.fishery.id", "fram.time.step"))
fishery.scalars <- arrange(fishery.scalars, fram.run.id, fram.fishery.id, fram.time.step)


person.fishery <- ReadCsv("PersonFramFisheries.csv", data.dir, unique.col.names=c("fram.fishery.id"))
fishery.scalars <- inner_join(fishery.scalars, person.fishery, by=c("fram.fishery.id"))

fram.run.id <- unique(fishery.scalars$fram.run.id)
fishery.scalars <- select(fishery.scalars, -one_of("fram.run.id"))

if (length(fram.run.id) > 1) {
  stop("ERROR - there is more then one run found, this is a major issue to debug")
}

###### Compile Escapement/Recruitment Data Frame  #####################
escapement <- left_join(stocks, backward.esc, by=c("fram.stock.id"))

person.stocks <- ReadCsv("PersonFramStocks.csv", data.dir, unique.col.names=c("fram.stock.id"))

person.stock.esc <- inner_join(person.stocks, escapement)


unique.person <- unique(person.fishery$person.name)
unique.person <- unique.person[nchar(unique.person) > 0]

for (person.name in unique.person) {
  person.fishery.scalars <- fishery.scalars[tolower(fishery.scalars$person.name) == tolower(person.name),]
  
  person.fishery.scalars <- select(person.fishery.scalars, 
                                   -one_of("fram.run.name", "person.name"))
  
  import.file.name <- sprintf("./report/%s_%s_%s.csv", person.name, fram.run.name, GetTimeStampText())
  
  cat(sprintf("Creating import file: %s\n", import.file.name))
  import.file <- file(import.file.name, "w+")
  
  cat(paste0("Person Name:", person.name, "\n"), file = import.file)
  cat(paste0("FRAM Run Name:", fram.run.name, "\n"), file = import.file)
  cat(paste0("FRAM Run ID:", fram.run.id, "\n"), file = import.file)
  cat(paste0("FRAM DB Name:", fram.db.name, "\n"), file = import.file)
  cat("-------------------------------------------------------------\n", file = import.file)
  
  tmp.file.name <- sprintf("./report/%s.tmp", person.name)
  WriteCsv(tmp.file.name, person.fishery.scalars)
  tmp.file <- file(tmp.file.name, "r")
  catch.csv.text <- readLines(con=tmp.file)
  cat(paste0(catch.csv.text, collapse="\n"), file = import.file)
  cat("\n-------------------------------------------------------------\n", file = import.file)
  tmp.file.name <- sprintf("./report/%s.tmp", person.name)
  WriteCsv(tmp.file.name, person.fishery.scalars)
  tmp.file <- file(tmp.file.name, "r")
  catch.csv.text <- readLines(con=tmp.file)
  cat(paste0(catch.csv.text, collapse="\n"), file = import.file)
 
  close(tmp.file)
  unlink(tmp.file.name)
  
  close(import.file)
}


