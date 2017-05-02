################
#
# Code to import template file that can be used to update a particular person's 
# catch and target escapement data.
#
# Nicholas Komick
# nicholas.komick@dfo-mpo.gc.ca
# February 9, 2016
# Using: http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html
#
################

rm(list=ls())   		#clean up the workspace
header <- "Create Import File Tool v0.3 beta"

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
source(file.path(source.lib.dir, "PscFramAdminData.r"))

required.packages <- c("RODBC", "dplyr")
InstallRequiredPackages(required.packages)

#' Write the import file template with provided data.
#'
#' @param person_name Person that the data is associated with
#' @param fram_run_name The name of the FRAM run
#' @param fram_run_id The ID of the FRAM run that is used for the import template
#' @param fram_db_name The MS Access file name that the FRAM model run is saved in
#' @param person_fishery_scalars A data frame with the person's fishery scalars
#' @param person_escapement A data frame with the person's target escapement for backwards FRAM
#'
#' 
WriteImportFile <- function (person_name, 
                             fram_run_name,
                             fram_run_id,
                             fram_db_name,
                             person_fishery_scalars, 
                             person_escapement) {
  
  import.file.name <- sprintf("./report/%s_%s_%s.csv", person_name, fram_run_name, GetTimeStampText())
  
  cat(sprintf("Creating import file: %s\n", import.file.name))
  import.file <- file(import.file.name, "w+")
  
  cat(paste0("Person Name:", person_name, "\n"), file = import.file)
  cat(paste0("FRAM Run Name:", fram_run_name, "\n"), file = import.file)
  cat(paste0("FRAM Run ID:", fram_run_id, "\n"), file = import.file)
  cat(paste0("FRAM DB Name:", fram_db_name, "\n"), file = import.file)
  cat("-------------------------------------------------------------\n", file = import.file)
  
  catch.csv.text <- WriteMemoryCsv(person_fishery_scalars)
  cat(paste0(catch.csv.text, collapse="\n"), file = import.file)
  
  if (nrow(person_escapement) > 0) {
    cat("\n-------------------------------------------------------------\n", file = import.file)
    esc.csv.text <- WriteMemoryCsv(person_escapement)
    cat(paste0(esc.csv.text, collapse="\n"), file = import.file)    
  }
  close(import.file)
}

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

base.fishery <- GetFramBaseFisheries(fram.db.conn, fram.run.name)

backward.esc <- GetFramBackwardEscapement(fram.db.conn, fram.run.name)
stock.recruit <- GetFramStockRecruitScalars(fram.db.conn, fram.run.name)
#drop the unnecessary FRAM Run ID
stock.recruit <- select(stock.recruit, -one_of("fram.run.id"))

stocks <- GetFramStocks(fram.db.conn)


odbcClose(fram.db.conn)

###### Compile Fishery Catch Data Frame  #####################
fishery.scalars <- left_join(base.fishery, fishery.scalars, by=c("fram.run.id", "fram.fishery.id", "fram.time.step"))
fishery.scalars <- arrange(fishery.scalars, fram.run.id, fram.fishery.id, fram.time.step)


person.fishery <- GetPersonFramFisheries()
fishery.scalars <- inner_join(fishery.scalars, person.fishery, by=c("fram.fishery.id"))

fram.run.id <- unique(fishery.scalars$fram.run.id)
fishery.scalars <- select(fishery.scalars, -one_of("fram.run.id"))

if (length(fram.run.id) > 1) {
  stop("ERROR - there is more then one run found, this is a major issue to debug")
}

###### Compile Escapement/Recruitment Data Frame  #####################
escapement <- left_join(stocks, backward.esc, by=c("fram.stock.id"))
escapement <- left_join(escapement, stock.recruit, by=c("fram.stock.id"))
escapement$target.escapement[is.na(escapement$target.escapement)] <- 0
escapement$escapement.flag[is.na(escapement$escapement.flag)] <- FramTargetNotUsedFlag
escapement$recruit.scalar[is.na(escapement$recruit.scalar)] <- 0
escapement$fram.run.id[is.na(escapement$fram.run.id)] <- fram.run.id


person.stocks <- GetPersonFramStocks()

escapement <- inner_join(escapement, person.stocks, by=c("fram.stock.id"))

unique.person <- unique(person.fishery$person.name)
unique.person <- unique.person[nchar(unique.person) > 0]

WriteImportFile("ALL", 
                fram.run.name,
                fram.run.id,
                fram.db.name,
                select(fishery.scalars, 
                       -one_of("fram.run.name", "person.name")), 
                select(escapement, 
                       -one_of("person.name", "fram.run.id", "run.year")))

for (this.person.name in unique.person) {
  person.fishery.scalars <- filter(fishery.scalars,
                                   tolower(person.name) == tolower(this.person.name))
  
  person.fishery.scalars <- select(person.fishery.scalars, 
                                   -one_of("fram.run.name", "person.name"))

  person.escapement <- filter(escapement, 
                              tolower(person.name) == tolower(this.person.name))
  
  person.escapement <- select(person.escapement, 
                              -one_of("person.name", "fram.run.id", "run.year"))
  
  WriteImportFile(this.person.name, 
                  fram.run.name,
                  fram.run.id,
                  fram.db.name,
                  person.fishery.scalars, 
                  person.escapement)
}


