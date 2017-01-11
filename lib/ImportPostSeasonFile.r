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
header <- "Import Post Season File Tool v0.1 beta"
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
  
  return (import.data)
}

ValidPostSeasonCatch <- function(fishery.scalars) {
  # Validates the catch data for parameterization of the FRAM model.
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
  
  inval.nonselect <- filter(fishery.scalars, nonselective.catch > 0, nonselective.flags != 1)
  if (nrow(inval.nonselect) > 0) {
    valid.catch <- FALSE
    inval.nonselect.fishery <- unique(select(inval.nonselect, fishery.name, fishery.id))
    cat("ERROR - The following non-selective fisheries have invalid flag, it should be 1 or 18.\n")
    fishery.txt <- paste(inval.nonselect.fishery$fishery.name, 
                         " (", 
                         inval.nonselect.fishery$fishery.id, 
                         ")", 
                         collapse=", ", sep="")
    cat(fishery.txt)
    cat("\n\n")
    
  }
  
  msf.flags <- as.integer(fishery.scalars$fishery.flag %% 10)
  inval.msf <- filter(fishery.scalars, msf.catch > 0, nonselective.flags != 8)
  if (nrow(inval.msf) > 0) {
    valid.catch <- FALSE
    inval.msf.fishery <- unique(select(inval.msf, fishery.name, fishery.id))
    cat("ERROR - The following MSF fisheries have invalid flag, it should be 8 or 18.\n")
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

required.packages <- c("RODBC", "dplyr")
InstallRequiredPackages(required.packages)

cat(header)

import.file.name <- choose.files(caption = "Select Import file", multi=FALSE, filters = Filters[c("txt", "All"),])

import.data <- ParseImportFile(import.file.name)

fram.db.name <- import.data$header$variable.value[toupper(import.data$header$variable.name) == "FRAM DB NAME"]
fram.run.name <- import.data$header$variable.value[toupper(import.data$header$variable.name) == "FRAM RUN NAME"]
fram.run.id <- as.numeric(import.data$header$variable.value[toupper(import.data$header$variable.name) == "FRAM RUN ID"])

cat("\n")
cat(sprintf("Updating FRAM database file: %s\n", fram.db.name))
cat(sprintf("Updating FRAM run name: %s\n", fram.run.name))
cat("\n")

if (ValidPostSeasonCatch(import.data$fishery.scalars) == FALSE) {
  stop("Post season catch data issues must be fixed before importing the data.")
}
  

fram.db.conn <- odbcConnectAccess(fram.db.name)

UpdateFisheryScalars(fram.db.conn, fram.run.id, import.data$fishery.scalars)

odbcClose(fram.db.conn)




