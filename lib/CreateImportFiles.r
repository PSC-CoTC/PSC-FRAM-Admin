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


person.fishery <- ReadCsv("PersonFramFisheries.csv", data.dir, unique.col.names=c("fram.fishery.id"))
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
escapement$escapement.flag[is.na(escapement$escapement.flag)] <- 0
escapement$recruit.scalar[is.na(escapement$recruit.scalar)] <- 0
escapement$fram.run.id[is.na(escapement$fram.run.id)] <- fram.run.id


person.stocks <- ReadCsv("PersonFramStocks.csv", data.dir, unique.col.names=c("fram.stock.id"))

escapement <- inner_join(escapement, person.stocks, by=c("fram.stock.id"))

unique.person <- unique(person.fishery$person.name)
unique.person <- unique.person[nchar(unique.person) > 0]

for (this.person.name in unique.person) {
  person.fishery.scalars <- filter(fishery.scalars,
                                   tolower(person.name) == tolower(this.person.name))
  
  person.fishery.scalars <- select(person.fishery.scalars, 
                                   -one_of("fram.run.name", "person.name"))

  person.escapement <- filter(escapement, 
                              tolower(person.name) == tolower(this.person.name))
  
  person.escapement <- select(person.escapement, 
                              -one_of("person.name", "fram.run.id", "run.year"))  
  
  import.file.name <- sprintf("./report/%s_%s_%s.csv", this.person.name, fram.run.name, GetTimeStampText())
  
  cat(sprintf("Creating import file: %s\n", import.file.name))
  import.file <- file(import.file.name, "w+")
  
  cat(paste0("Person Name:", this.person.name, "\n"), file = import.file)
  cat(paste0("FRAM Run Name:", fram.run.name, "\n"), file = import.file)
  cat(paste0("FRAM Run ID:", fram.run.id, "\n"), file = import.file)
  cat(paste0("FRAM DB Name:", fram.db.name, "\n"), file = import.file)
  cat("-------------------------------------------------------------\n", file = import.file)
  
  tmp.file.name <- sprintf("./report/%s.tmp", this.person.name)
  catch.csv.text <- WriteMemoryCsv(person.fishery.scalars)
  cat(paste0(catch.csv.text, collapse="\n"), file = import.file)
  
  if (nrow(person.escapement) > 0) {
    cat("\n-------------------------------------------------------------\n", file = import.file)
    esc.csv.text <- WriteMemoryCsv(person.escapement)
    cat(paste0(esc.csv.text, collapse="\n"), file = import.file)    
  }

  unlink(tmp.file.name)
  
  close(import.file)
}


