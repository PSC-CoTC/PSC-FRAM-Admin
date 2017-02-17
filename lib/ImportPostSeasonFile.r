################
#
# Code to import a file that updates a particular person's catch data.
#
# Nicholas Komick
# nicholas.komick@dfo-mpo.gc.ca
# January 9, 2017
# Using: http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html
#
################

rm(list=ls())   		#clean up the workspace
header <- "Import Post Season File Tool v0.2"
options(stringsAsFactors = FALSE)

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

ParseImportFile <- function(import.file.name) {
  # Parses the import file format and returns the results into a list
  # The file format generally consists of a header followed by a CSV table of fishery catch.
  # Each section of the file is seperated by 4 or more dashes (e.g. ----------) and a carrage return.
  #
  # Args:
  #   import.file.name: The file name of the import file
  #
  # Returns:
  #   A list with the different sections of the 
  #
  # Exceptions:
  #   None
  #   
  import.data <- list()
  file.text <- readChar(import.file.name, file.info(import.file.name)$size)
  
  sections <- strsplit(file.text, "[-]{4,}")[[1]]
  
  header <- sections[1]
  
  import.data$header <- read.table(textConnection(header), sep = ":", header=FALSE)
  names(import.data$header) <- c("variable.name", "variable.value")
  
  catch <- sections[2]
  while (substr(catch, 1, 1) %in% c("\n", "\r")) {
    #strip blank lines from before the catch data, so that the first line is the table header
    catch <- substring(catch, 2)
  }

  import.data$fishery.scalars <- read.table(textConnection(catch), sep = ",", header=TRUE)
  
  na.msf <- is.na(import.data$fishery.scalars$msf.catch)
  import.data$fishery.scalars$msf.catch[na.msf] <- 0
  
  return (import.data)
}

ValidPostSeasonCatch <- function(fishery.scalars) {
  # Validates the catch data for parametrization of the FRAM model.
  # Most of the validation is related to the appropriate setting of flags and 
  # providing all the necessary parameters.
  #
  # Args:
  #   catch.data: The catch data loaded from a post season import file.
  #
  # Returns:
  #   A boolean, TRUE for valid or FALSE for when there is issues with the catch
  #
  # Exceptions:
  #   None
  #   
  valid.catch <- TRUE
  
  nonselective.flags <- as.integer(fishery.scalars$fishery.flag  / 10)
  nonselective.flags[fishery.scalars$fishery.flag < 10] <- 
    fishery.scalars$fishery.flag[fishery.scalars$fishery.flag < 10]
  
  inval.nonselect <- filter(fishery.scalars, nonselective.catch > 0, 
                            nonselective.flags != kFramNonSelectiveQuotaFlag)
  if (nrow(inval.nonselect) > 0) {
    valid.catch <- FALSE
    inval.nonselect.fishery <- unique(select(inval.nonselect, fishery.name, fishery.id))
    
    cat(sprintf("ERROR - The following non-selective fisheries have invalid flag, it should be %d or %d.\n",
                kFramNonSelectiveQuotaFlag,
                kFramNonSelectiveQuotaFlag * 10 + kFramMsfQuotaFlag))
    
    fishery.txt <- paste(inval.nonselect.fishery$fishery.name, 
                         " (", 
                         inval.nonselect.fishery$fishery.id, 
                         ")", 
                         collapse=", ", sep="")
    cat(fishery.txt)
    cat("\n\n")
  }
  
  msf.flags <- as.integer(fishery.scalars$fishery.flag %% 10)
  inval.msf <- filter(fishery.scalars, msf.catch > 0, 
                      msf.flags != kFramMsfQuotaFlag)
  
  if (nrow(inval.msf) > 0) {
    valid.catch <- FALSE
    inval.msf.fishery <- unique(select(inval.msf, fishery.name, fishery.id))
    cat(sprintf("ERROR - The following MSF fisheries have invalid flag, it should be %d or %d.\n",
                kFramMsfQuotaFlag,
                kFramNonSelectiveQuotaFlag * 10 + kFramMsfQuotaFlag))
    fishery.txt <- paste(inval.msf.fishery$fishery.name, 
                         " (", 
                         inval.msf.fishery$fishery.id, 
                         ")", 
                         collapse=", ", sep="")
    cat(fishery.txt)
    cat("\n\n")    
  }

  return (valid.catch)
}

ValidMarkInfo <- function(fishery.scalars) {
  # Validates the mark rate information for mark-selective fisheries.
  #
  # Args:
  #   fishery.scalars: The catch data loaded from a post season import file.
  #
  # Returns:
  #   A boolean, TRUE for valid or FALSE for when there is issues with the mark rate information
  #   with the mark-selective fisheries
  #
  # Exceptions:
  #   None
  #   
  
  valid.mark.info <- TRUE

  msf.flags <- as.integer(fishery.scalars$fishery.flag %% 10)
  inval.mark.info <- filter(fishery.scalars,
                            msf.flags %in% c(kFramMsfScalarFlag, kFramMsfQuotaFlag),
                            !(mark.release.rate > 0),
                            !(mark.missid.rate > 0),
                            !(unmark.missid.rate > 0),
                            !(mark.incidental.rate > 0))
  
  if (nrow(inval.mark.info) > 0) {
    valid.mark.info <- FALSE
    inval.msf.fishery <- unique(select(inval.mark.info, fishery.name, fishery.id))
    cat(sprintf("ERROR - The following MSF fisheries must have mark rate information with fishery flags %d or %d.\n",
                kFramMsfQuotaFlag,
                kFramNonSelectiveQuotaFlag * 10 + kFramMsfQuotaFlag))
    fishery.txt <- paste(inval.msf.fishery$fishery.name, 
                         " (", 
                         inval.msf.fishery$fishery.id, 
                         ")", 
                         collapse=", ", sep="")
    cat(fishery.txt)
    cat("\n\n")    
  }
  
  
  return (valid.mark.info)
}

ValidFisheries <- function(person.name, fram.db.conn, fram.run.name, fishery.scalars) {
  # Validates the fishery definitions for parametrization of the FRAM model.
  # This function checks that all the fisheries are valid, relative to the base period
  # and that all the fisheries identified are the responsibility of the identified person.
  #
  # Args:
  #   person.name: the name of the person for the import file
  #   fram.db.conn: FRAM database connection (e.g. ODBC)
  #   fram.run.name: FRAM run name from the import file
  #   fishery.scalars: fishery catch data provided from the import file
  #
  # Returns:
  #   A boolean, TRUE for valid or FALSE for when there is issues with the catch
  #
  # Exceptions:
  #   None
  #   
  
  is.valid.fisheries <- TRUE
  
  base.fishery <- GetRunBaseFisheries(fram.db.conn, fram.run.name)
  
  person.fishery <- ReadCsv("PersonFisheries.csv", data.dir, unique.col.names=c("fishery.id"))
  
  valid.fishery <- inner_join(person.fishery, base.fishery, by=c("fishery.id"))
  
  valid.fishery <- valid.fishery[valid.fishery$person.name == person.name,]
  
  valid.fishery <- select(valid.fishery, fishery.id, time.step)
  
  import.fishery <- select(fishery.scalars, fishery.id, time.step)
  import.fishery <- unique(import.fishery)
  
  inapprop.fisheries <- setdiff(import.fishery, valid.fishery)
  if (nrow(inapprop.fisheries) > 0) {
    is.valid.fisheries <- FALSE
    fishery.names <- select(base.fishery, fishery.id, fishery.name)
    fishery.names <- distinct(fishery.names)
    inapprop.fisheries <- inner_join(inapprop.fisheries, fishery.names, by=c("fishery.id"))
    cat("The following fisheries/time steps are inappropriately defined (e.g. not valid to base period or not assign to the person)\n\n")
    error.msg <- paste(inapprop.fisheries$fishery.name, 
                        " (", 
                        inapprop.fisheries$fishery.id, 
                        ") - ", 
                        inapprop.fisheries$time.step,
                        sep="", collapse="\n")
    cat(error.msg)
    cat("\n\n")
  }
  
  missing.fisheries <- setdiff(valid.fishery, import.fishery)
  if (nrow(missing.fisheries) > 0) {
    is.valid.fisheries <- FALSE
    fishery.names <- select(base.fishery, fishery.id, fishery.name)
    fishery.names <- distinct(fishery.names)
    missing.fisheries <- inner_join(missing.fisheries, fishery.names, by=c("fishery.id"))
    cat("The following fisheries/time steps are missing from the import (e.g. assigned to the person, but not in the import file)\n\n")
    error.msg <- paste(missing.fisheries$fishery.name, 
                       " (", 
                       missing.fisheries$fishery.id, 
                       ") - ", 
                       missing.fisheries$time.step,
                       sep="", collapse="\n")
    cat(error.msg)
    cat("\n\n")
  }  

  return (is.valid.fisheries)
}

required.packages <- c("RODBC", "dplyr")
InstallRequiredPackages(required.packages)

cat(header)

config.file.name <- NA
cmdArgs <- commandArgs(TRUE)
if(length(cmdArgs) > 0) {
  print(cmdArgs)
  config.file.name <- cmdArgs[1]
  cat(sprintf("Using configuration file '%s'\n", config.file.name))
} else {
  config.file.name <- "./config/import_post_season_config.r"
  cat(sprintf("WARNING - configuration file not provided, default file '%s' is used.\n", config.file.name))
}

LoadConfigFiles(report.config.file=config.file.name)

import.file.name <- choose.files(caption = "Select Import file", multi=FALSE, filters = Filters[c("txt", "All"),])

import.data <- ParseImportFile(import.file.name)

fram.db.name <- import.data$header$variable.value[toupper(import.data$header$variable.name) == "FRAM DB NAME"]
fram.run.name <- import.data$header$variable.value[toupper(import.data$header$variable.name) == "FRAM RUN NAME"]
fram.run.id <- as.numeric(import.data$header$variable.value[toupper(import.data$header$variable.name) == "FRAM RUN ID"])
person.name <- as.character(import.data$header$variable.value[toupper(import.data$header$variable.name) == "PERSON NAME"])

cat("\n")
cat(sprintf("Updating FRAM database file: %s\n", fram.db.name))
cat(sprintf("Updating FRAM run name: %s\n", fram.run.name))
cat("\n")

error.found <- FALSE

if (exists("validate.catch") == FALSE || validate.catch == TRUE) {
  if (ValidPostSeasonCatch(import.data$fishery.scalars) == FALSE) {
    error.found <- TRUE
  } 
}

if (exists("validate.mark.info") == FALSE || validate.mark.info == TRUE) {
  if (ValidMarkInfo(import.data$fishery.scalars) == FALSE) {
    error.found <- TRUE
  } 
}


fram.db.conn <- odbcConnectAccess(fram.db.name)

if (exists("validate.fisheries") == FALSE || validate.fisheries == TRUE) {
  if (ValidFisheries(person.name,
                     fram.db.conn,
                     fram.run.name,
                     import.data$fishery.scalars) == FALSE) {
    error.found <- TRUE
  } 
}

if (error.found) {
  stop("Issues with the post season import file must be fixed before being imported")
} else {
  UpdateFisheryScalars(fram.db.conn, fram.run.id, import.data$fishery.scalars)
}

odbcClose(fram.db.conn)




