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
  cat("No command line parameters provided.\n")
}

cat(header)
cat("\n")


import.file.name <- choose.files(caption = "Select Import file", multi=FALSE, filters = Filters[c("txt", "All"),])

import.data <- ParseImportFile(import.file.name)

fram.db.name <- import.data$header$variable.value[toupper(import.data$header$variable.name) == "FRAM DB NAME"]

fram.db.conn <- odbcConnectAccess(fram.db.name)

fram.run.id <- as.numeric(import.data$header$variable.value[toupper(import.data$header$variable.name) == "FRAM RUN ID"])

UpdateFisheryScalars(fram.db.conn, import.data$fishery.scalars)

odbcClose(fram.db.conn)


# 
# 
# fram.db.conn <- odbcConnectAccess(fram.db.name)
# fishery.scalars <- GetFisheryScalars(fram.db.conn, fram.run.name)
# 
# 
# 
# person.fishery <- ReadCsv("PersonFisheries.csv", data.dir, unique.col.names=c("fishery.id"))
# fishery.scalars <- merge(fishery.scalars, person.fishery, by=c("fishery.id"))
# 
# fram.run.id <- unique(fishery.scalars$run.id)
# if (length(fram.run.id) > 1) {
#   stop("ERROR - there is more then one run found, this is a major issue to debug")
# }
# 
# unique.person <- unique(person.fishery$person.name)
# unique.person <- unique.person[nchar(unique.person) > 0]
# 
# for (person.name in unique.person) {
#   person.fishery.scalars <- fishery.scalars[tolower(fishery.scalars$person.name) == tolower(person.name),]
#   person.fishery.scalars <- person.fishery.scalars[ , names(person.fishery.scalars) %notin% c("run.name", "person.name")]
#   import.file.name <- sprintf("./report/%s catch.csv", person.name)
#   
#   import.file <- file(import.file.name, "w+")
#   
#   cat(paste0("Person Name:", person.name, "\n"), file = import.file)
#   cat(paste0("FRAM Run Name:", fram.run.name, "\n"), file = import.file)
#   cat(paste0("FRAM Run ID:", fram.run.id, "\n"), file = import.file)
#   cat(paste0("FRAM DB Name:", fram.db.name, "\n"), file = import.file)
#   cat(paste0("Person Name:", person.name, "\n"), file = import.file)
#   cat("-------------------------------------------------------------\n", file = import.file)
#   
#   tmp.file.name <- sprintf("./report/%s.tmp", person.name)
#   WriteCsv(tmp.file.name, person.fishery.scalars)
#   tmp.file <- file(tmp.file.name, "r")
#   catch.csv.text <- readLines(con=tmp.file)
#   cat(paste0(catch.csv.text, collapse="\n"), file = import.file)
#   close(tmp.file)
#   unlink(tmp.file.name)
#   
#   close(import.file)
# }


