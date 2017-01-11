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
header <- "Import Agency File Tool v0.1 alpha"
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

required.packages <- c("RODBC")
InstallRequiredPackages(required.packages)

cmdArgs <- commandArgs(TRUE)
if(length(cmdArgs) > 0) {
  print(cmdArgs)
} else {
  #cat("No command line parameters provided.\n")
}

cat(header)


import.file.name <- choose.files(caption = "Select Import file", multi=FALSE, filters = Filters[c("txt", "All"),])

import.data <- ParseImportFile(import.file.name)

fram.db.name <- import.data$header$variable.value[toupper(import.data$header$variable.name) == "FRAM DB NAME"]
fram.run.name <- import.data$header$variable.value[toupper(import.data$header$variable.name) == "FRAM RUN NAME"]
fram.run.id <- as.numeric(import.data$header$variable.value[toupper(import.data$header$variable.name) == "FRAM RUN ID"])

cat("\n")
cat(sprintf("Use db file: %s\n", fram.db.name))
cat(sprintf("Use run name: %s\n", fram.run.name))
cat("\n")

fram.db.conn <- odbcConnectAccess(fram.db.name)

UpdateFisheryScalars(fram.db.conn, fram.run.id, import.data$fishery.scalars)

odbcClose(fram.db.conn)




