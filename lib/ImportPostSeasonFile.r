################
#
# Utility to import a file that updates a particular person's catch and escapement
# data for FRAM post-season model run.
#
# Nicholas Komick
# nicholas.komick@dfo-mpo.gc.ca
# January 9, 2017
# Using style: http://style.tidyverse.org/
#
################

rm(list=ls())   		#clean up the workspace
header <- "Import Post Season File Tool v0.3b"
options(stringsAsFactors = FALSE)


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

#' Parses the import file format and returns the results into a list
#' The file format generally consists of a header followed by a CSV table of fishery catch.
#' Each section of the file is seperated by 4 or more dashes (e.g. ----------) and a carrage return.
#'
#' @param import.file.name The file name of the import file
#'
#' @return A list with the different sections of the import file
#' 
ParseImportFile <- function(import.file.name) {
  import.data <- list()
  file.text <- readChar(import.file.name, file.info(import.file.name)$size)
  
  file.text %<>%str_replace_all("[,]{4,}[\r]?\n", "\n")
  
   sections <- strsplit(file.text, "[\r]?\n[-]{4,}[\r]?\n")[[1]]
   # sections <- strsplit(file.text,  "[\r]?\n[-]{4,}[,]{0,}")[[1]] #MOB - didnt't work try this way
  
  
  header <- sections[1]
  
  import.data$header <- read_delim(header, 
                                   ":", 
                                   col_names=c("variable.name", "variable.value"))
  
  if (length(sections) > 1){
    for (section_idx in 2:length(sections)) {
      data_table <- sections[section_idx]
      while (substr(data_table, 1, 1) %in% c("\n", "\r", ",")) { #mob add in the ","
        #strip blank lines from before the catch data, so that the first line is the table header
        data_table <- substring(data_table, 2)
      }
      #Add a new line character so that the file parses correctly
      data_table <- paste0(data_table, "\r\n", collapse="")
      
      data_table <- ReadMemoryCsv(data_table)
      
      first_col_name <- names(data_table)[1]
      
      if (first_col_name %in% c("fram.fishery.id", "fram.fishery.name")) {
        import.data$fishery.scalars <- data_table
      } else if (first_col_name %in% c("fram.stock.id", "fram.stock.name")) {
        import.data$target.escapement <- data_table
      } else {
        stop(sprintf("Unknown data table in the import file with the first column name '%s'",
                     first_col_name))
      }
    }
    
  } else {
    stop("There is no data in the import file to import.")
  }
  
  if (!is.null(import.data$fishery.scalars)) {
    #remove blank lines
    import.data$fishery.scalars <- filter(import.data$fishery.scalars,
                                          !is.na(fram.fishery.id) & 
                                            !is.na(fram.time.step) &
                                            !is.na (fishery.flag))
    
    na.msf <- is.na(import.data$fishery.scalars$msf.catch)
    import.data$fishery.scalars$msf.catch[na.msf] <- 0
    
    if ("comment.cnr" %notin% str_to_lower(names(import.data$fishery.scalars))) {
      #Add the comment column if missing
      import.data$fishery.scalars$comment.cnr <- ""
    } else {
      import.data$fishery.scalars <-
         mutate(import.data$fishery.scalars,
                comment.cnr = as.character(comment.cnr))
    }
    
    if ("comment.catch" %notin% str_to_lower(names(import.data$fishery.scalars))) {
      #Add the comment column if missing
      import.data$fishery.scalars$comment.catch <- ""
    } else {
      import.data$fishery.scalars <-
        mutate(import.data$fishery.scalars,
          comment.catch = as.character(comment.catch))
    
    }
  }
  
  
  
  if (!is.null(import.data$target.escapement)) {
    #remove blank lines
    import.data$target.escapement <- 
        filter(import.data$target.escapement,
               !is.na(fram.stock.id) & !is.na(escapement.flag)) %>%
        mutate(escapement.flag = as.integer(escapement.flag))
    
    if ("comment" %notin% str_to_lower(names(import.data$target.escapement))) {
      import.data$target.escapement$comment <- ""
    }
  }

  return (import.data)
}

#' Validates the escapement flags against the escapement values for parametrization 
#' of the backward FRAM model run.
#'
#' @param target_escapement A data frame of the targent escapements from an import file
#'
#' @return A boolean, TRUE for valid or FALSE for when there is issues with the escapement flags
#'
ValidEscapementFlags <- function(target_escapement) {
  valid.esc <- TRUE
  
  esc.required <- filter(target_escapement, 
                         escapement.flag == FramTargetEscExactFlag & 
                           target.escapement == 0)
  
  if (nrow(esc.required) > 0) {
    
    cat(sprintf("WARNING - The following stocks have a target escapement of ZERO.  You may wish to change the escapement flag to %d if you do not know the escapement target.\n\n",
                FramTargetNotUsedFlag))
    
    
  esc_txt <- FormatNameIdText(esc.required$fram.stock.name, 
                                esc.required$fram.stock.id)

    cat(esc_txt)
    cat("\n\n")
  }  
  
  esc.notused <- filter(target_escapement,
                        escapement.flag == FramTargetNotUsedFlag & target.escapement > 0)
  
  if (nrow(esc.notused) > 0) {
    valid.esc <- FALSE
    
    cat(sprintf("ERROR - The following stocks have target escapement but the flag identfies it as not specified.  To suggested fix is to change the escapement flag should possibly be %d.\n\n",
                FramTargetEscExactFlag))
    
    esc_txt <- FormatNameIdText(esc.notused$fram.stock.name, 
                                esc.notused$fram.stock.id)
    cat(esc_txt)
    cat("\n\n")
  }  
  
  return(valid.esc)
}

#' Matches escapment flags for marked and unmarked stocks, to validate escapement flags
#' and identify if stock recruit scalars need to be updated.
#'
#' @param target_escapement A data frame of the targent escapements from an import file
#'
#' @return The target_escapement with an additional column identify marked/unmarkd combined flag
#'
PairEscapementFlags <- function(target_escapement) {
  valid_esc <- TRUE
  marked_esc_df <- filter(target_escapement, fram.stock.id %% 2 == 0)
  unmarked_esc_df <- filter(target_escapement, fram.stock.id %% 2 == 1)
  
  unmarked_esc_df <- mutate(unmarked_esc_df,
                            fram.mark.stock.id = fram.stock.id + 1)
  
  esc_pair_df <- full_join(marked_esc_df, 
                           unmarked_esc_df, 
                           by = c("fram.stock.id" = "fram.mark.stock.id"),
                           suffix = c(".m", ".u"))  
  
  #In an import file, both the marked & unmarked stock must be provided
  pair_mismatch <- filter(esc_pair_df, is.na(fram.stock.id) | is.na(fram.stock.id.u))
  if (nrow(pair_mismatch) > 0) {
    valid_esc <- FALSE
    error.msg <- paste(coalesce(pair_mismatch$fram.stock.name.m, 
                                pair_mismatch$fram.stock.name.u), 
                       sep="", collapse="\n")
    cat("ERROR - The following stocks only have just marked/unmarked stock specified but missing other mark status stock.\n",
        "Fix this by providing both the marked and unmarked stock definitions in the import file.\n\n",
        error.msg)
  }
  
  #The Mark/Unmark split escapement flag should only appear on the marked stock
  unmarked_split <- filter(esc_pair_df, escapement.flag.u == FramTargetEscSplitFlag)
  if (nrow(unmarked_split) > 0) {
    valid_esc <- FALSE
    stock_list_msg <- paste(unmarked_split$fram.stock.name.u, 
                            sep="", 
                            collapse="\n")
    cat("ERROR - The following unmarked stocks have the split mark/unmark escapement flag on the unmarked stock.\n",
        sprintf("Fix this by providing %d escapement flag on the unmarked and %d on the marked stock.\n\n",
                FramTargetNotUsedFlag,
                FramTargetEscSplitFlag),
        stock_list_msg)
  }  
  
  split_w_unmarked <- filter(esc_pair_df, escapement.flag.u != 0 & escapement.flag.m == 2)
  if (nrow(split_w_unmarked) >0){
    
    valid_esc <- FALSE
    stock_list_msg <- paste(split_w_unmarked$fram.stock.name.u,
      sep="", 
      collapse="\n")
    
    valid_esc <- FALSE
    flag_list_msg <- paste(split_w_unmarked$escapement.flag.u,
      sep="/", 
      collapse="\n")
    
      cat(sprintf("ERROR - The following stocks have the split mark/unmark escapement flag (%d) but a unmarked flag is not %d.\n",
        FramTargetEscSplitFlag,
        FramTargetNotUsedFlag,
        flag_list_msg))
  }
  
  if(valid_esc == FALSE) {
    stop("The marked/unmarked escapement issues must be fixed before the file can be imported.")
  }

  
  #The next section of code pairs the Mark/Unmark Split escapement flag with the unmarked and marked
  # stocks.  This is needed else where in the code to identify if the target escapement or the recruitment
  # scalar needs to be updated.
  
  unmark_pair_df <- transmute(esc_pair_df,
    fram.stock.id = fram.stock.id.u,
    escapement.flag.u = if_else(escapement.flag.u == FramTargetNotUsedFlag &
        escapement.flag.m == FramTargetEscSplitFlag,
      escapement.flag.m,
      escapement.flag.u)) ##check
  
  target_escapement <- left_join(target_escapement, unmark_pair_df, by=c("fram.stock.id"))
  
  
  target_escapement <- mutate(target_escapement,
    pair_esc_flag = if_else(is.na(escapement.flag.u), 
      escapement.flag,
      escapement.flag.u))
  
  target_escapement <- select(target_escapement, -one_of(c("escapement.flag.u")))
  
  return(target_escapement)
}

#' Validates the catch data for parametrization of a FRAM post-season model run.
#' Most of the validation is related to the appropriate setting of flags and 
#' providing all the necessary parameters.
#'
#' @param fishery.scalars The catch data loaded from a post season import file.
#'
#' @return A boolean, TRUE for valid or FALSE for when there is issues with the catch
#'   
ValidPostSeasonCatch <- function(fishery.scalars) {

  valid.catch <- TRUE
  
  nonselective.flags <- as.integer(fishery.scalars$fishery.flag  / 10)
  nonselective.flags[fishery.scalars$fishery.flag < 10] <- 
    fishery.scalars$fishery.flag[fishery.scalars$fishery.flag < 10]
  
  inval.nonselect <- filter(fishery.scalars, nonselective.catch > 0, 
                            nonselective.flags != kFramNonSelectiveQuotaFlag)
  if (nrow(inval.nonselect) > 0) {
    valid.catch <- FALSE
    inval.nonselect.fishery <- unique(select(inval.nonselect, fram.fishery.name, fram.fishery.id))
    
    cat(sprintf("ERROR - The following non-selective fisheries have invalid flag, it should be %d or %d.\n",
                kFramNonSelectiveQuotaFlag,
                kFramNonSelectiveQuotaFlag * 10 + kFramMsfQuotaFlag))
    
    fishery_txt <- FormatNameIdText(inval.nonselect.fishery$fram.fishery.name, 
                                    inval.nonselect.fishery$fram.fishery.id)
    cat(fishery_txt)
    cat("\n\n")
  }
  
  msf.flags <- as.integer(fishery.scalars$fishery.flag %% 10)
  inval.msf <- filter(fishery.scalars, msf.catch > 0, 
                      msf.flags != kFramMsfQuotaFlag)
  
  if (nrow(inval.msf) > 0) {
    valid.catch <- FALSE
    inval.msf.fishery <- unique(select(inval.msf, fram.fishery.name, fram.fishery.id))
    cat(sprintf("ERROR - The following MSF fisheries have invalid flag, it should be %d or %d.\n",
                kFramMsfQuotaFlag,
                kFramNonSelectiveQuotaFlag * 10 + kFramMsfQuotaFlag))
    
    fishery_txt <- FormatNameIdText(inval.msf.fishery$fram.fishery.name,
                                    inval.msf.fishery$fram.fishery.id)
    cat(fishery_txt)
    cat("\n\n")    
  }

  return (valid.catch)
}

#' Validates the mark rate information for mark-selective fisheries.
#'
#' @param fishery.scalars The catch data loaded from a post season import file.
#'
#' @return A boolean, TRUE for valid or FALSE for when there is issues with the mark rate information
#'   with the mark-selective fisheries
#'
ValidMarkInfo <- function(fishery.scalars) {

  valid.mark.info <- TRUE

  msf.flags <- as.integer(fishery.scalars$fishery.flag %% 10)
  inval.mark.info <- filter(fishery.scalars,
                            msf.flags %in% c(kFramMsfScalarFlag, kFramMsfQuotaFlag),
                            !(mark.release.rate > 0),
                            !(mark.missid.rate > 0),
                            !(unmark.missid.rate > 0),
                            !(mark.incidental.rate > 0))
  
  if (nrow(inval.mark.info) > 0) {
    inval.msf.fishery <- unique(select(inval.mark.info, fram.fishery.name, fram.fishery.id))
    cat(sprintf("WARNING - The following MSF fisheries must have mark rate information with fishery flags %d or %d.\n\n",
                kFramMsfQuotaFlag,
                kFramNonSelectiveQuotaFlag * 10 + kFramMsfQuotaFlag))
    
    fishery_txt <- FormatNameIdText(inval.msf.fishery$fram.fishery.name,
                                    inval.msf.fishery$fram.fishery.id)
    cat(fishery_txt)
    cat("\n\n")    
  }
  
  return (valid.mark.info)
}


#' Validates the fishery definitions for parametrization of the FRAM model.
#' This function checks that all the fisheries are valid, relative to the base period
#' and that all the fisheries identified are the responsibility of the identified person.
#'
#' @param person.name the name of the person for the import file
#' @param fram.db.conn FRAM ODBC database connection 
#' @param fram.run.name FRAM run name from the import file
#' @param fishery.scalars fishery catch data provided from the import file
#'
#' @return A boolean, TRUE for valid or FALSE for when there is issues with the catch
#'
ValidFisheries <- function(person.name, fram.db.conn, fram.run.name, fishery.scalars) {
  
  is.valid.fisheries <- TRUE
  
  base.fishery <- GetFramBaseFisheries(fram.db.conn, fram.run.name)
  
  if (toupper(person.name) != "ALL") {
    person.fishery <- GetPersonFramFisheries(person.name)
  } else {
    person.fishery <- GetPersonFramFisheries()
  }
  
  valid.fishery <- inner_join(person.fishery, base.fishery, by=c("fram.fishery.id"))
  
  valid.fishery <- select(valid.fishery, fram.fishery.id, fram.time.step)
  
  import.fishery <- select(fishery.scalars, fram.fishery.id, fram.time.step)
  import.fishery <- distinct(import.fishery)
  
  inapprop.fisheries <- setdiff(import.fishery, valid.fishery)
  if (nrow(inapprop.fisheries) > 0) {
    is.valid.fisheries <- FALSE
    fishery.names <- select(base.fishery, fram.fishery.id, fram.fishery.name)
    fishery.names <- distinct(fishery.names)
    inapprop.fisheries <- inner_join(inapprop.fisheries, fishery.names, by=c("fram.fishery.id"))
    cat("ERROR - The following fisheries/time steps are inappropriately defined (e.g. not valid to base period or not assign to the person)\n\n")
    
    error.msg <- paste(inapprop.fisheries$fram.fishery.name, 
                        " (", 
                        inapprop.fisheries$fram.fishery.id, 
                        ") - ", 
                        inapprop.fisheries$fram.time.step,
                        sep="", collapse="\n")
    cat(error.msg)
    cat("\n\n")
  }
  
  missing.fisheries <- setdiff(valid.fishery, import.fishery)
  if (nrow(missing.fisheries) > 0) {
    is.valid.fisheries <- FALSE
    fishery.names <- select(base.fishery, fram.fishery.id, fram.fishery.name)
    fishery.names <- distinct(fishery.names)
    missing.fisheries <- inner_join(missing.fisheries, fishery.names, by=c("fram.fishery.id"))
    cat("ERROR - The following fisheries/time steps are missing from the import (e.g. assigned to the person, but not in the import file)\n\n")
    error.msg <- paste(missing.fisheries$fram.fishery.name, 
                       " (", 
                       missing.fisheries$fram.fishery.id, 
                       ") - ", 
                       missing.fisheries$fram.time.step,
                       sep="", collapse="\n")
    cat(error.msg)
    cat("\n\n")
  }  

  return (is.valid.fisheries)
}

#' Validates the target escapement definitions for parametrization of the FRAM model.
#' This function checks that all the stocks are valid, relative to the base period
#' and that all the stocks identified are the responsibility of the identified person.
#'
#' @param person_name the name of the person for the import file
#' @param fram_db_conn FRAM ODBC database connection 
#' @param fram_run_name FRAM run name from the import file
#' @param target_escapement Target escapement for a Backward FRAM model run
#'
#' @return A boolean, TRUE for valid or FALSE for when there is issues with the escapement
#'
ValidTargetEscapement <- function(person_name, fram_db_conn, fram_run_name, target_escapement) {

  is.valid.esc <- TRUE
  
  base.stock <- GetFramBaseStocks(fram_db_conn, fram_run_name)
  stock_names <- GetFramStocks(fram_db_conn)
  
  if (toupper(person_name) != "ALL") {
    person.stock <- GetPersonFramStocks(person_name)
  } else {
    person.stock <- GetPersonFramStocks()
  }
    
  valid.stock <- inner_join(person.stock, base.stock, by=c("fram.stock.id"))
    
  valid.stock <- select(valid.stock, fram.stock.id)
  
  import.stock <- select(target_escapement, fram.stock.id)
  import.stock <- distinct(import.stock)
  
  inapprop.stocks <- setdiff(import.stock, valid.stock)
  if (nrow(inapprop.stocks) > 0) {
    is.valid.esc <- FALSE
    inapprop.stocks <- inner_join(inapprop.stocks, stock_names, by=c("fram.stock.id"))
    cat("ERROR - The following stock(s) are inappropriately defined (e.g. not valid to base period or not assign to the person)\n\n")
    
    error_msg <- FormatNameIdText(inapprop.stocks$fram.stock.name,
                                  inapprop.stocks$fram.stock.id)
    cat(error_msg)
    cat("\n\n")
  }
  
  missing.stocks <- setdiff(valid.stock, import.stock)
  if (nrow(missing.stocks) > 0) {
    is.valid.esc <- FALSE
    stock.names <- select(base.stock, fram.stock.id, fram.stock.name)
    missing.stocks <- inner_join(missing.stocks, stock.names, by=c("fram.stock.id"))
    cat("ERROR - The following stock(s) are missing from the import (e.g. assigned to the person, but not in the import file)\n\n")

    error_msg <- FormatNameIdText(missing.stocks$fram.stock.name,
                                  missing.stocks$fram.stock.id)    

    cat(error_msg)
    cat("\n\n")
  }  
  
  
  # split_flag_issue <- filter(target_escapement, pair_esc_flag == 1 & escapement.flag == 0 & recruit.scalar > 0) ##what
  # if (nrow(split_flag_issue) > 0) {
  #   is.valid.esc <- FALSE
  #   stock.names <- select(split_flag_issue, fram.stock.id, fram.stock.name)
  #   cat("ERROR - The following stock(s) should have a split mark/unmark flag on thier marked release\n\n")
  #   
  #   error_msg <- FormatNameIdText(split_flag_issue$fram.stock.name,
  #                                 split_flag_issue$fram.stock.id)
  #   cat(error_msg)
  #   cat("\n\n")
  # }  
  
  return (is.valid.esc)
}


#start implemeneting

required.packages <- c("RODBC", "dplyr", "stringr", "magrittr")
InstallRequiredPackages(required.packages)

cat(header)
cat("\n\n")

config.file.name <- NA
cmdArgs <- commandArgs(TRUE)

if(length(cmdArgs) > 0) {
  config.file.name <- cmdArgs[1]
  cat(sprintf("Using configuration file: %s\n", config.file.name))
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

cat(sprintf("Updating FRAM database file:\n%s\n\n", fram.db.name))
cat(sprintf("Updating FRAM run name: %s\n", fram.run.name))
cat("\n")

error.found <- FALSE

if (!is.null(import.data$fishery.scalar)) {
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
}

if (exists("validate.escapment.flags") == FALSE || validate.escapment.flags == TRUE) {
  if (!is.null(import.data$target.escapement)) {
    if (ValidEscapementFlags(import.data$target.escapement) == FALSE) {
      error.found <- TRUE
    }
    import.data$target.escapement <- PairEscapementFlags(import.data$target.escapement)
  }
}

fram.db.conn <- odbcConnectAccess(fram.db.name)

CheckFramCommentCol(fram.db.conn)

if (!is.null(import.data$fishery.scalar)) {
  if (exists("validate.fisheries") == FALSE || validate.fisheries == TRUE) {
    if (ValidFisheries(person.name,
                       fram.db.conn,
                       fram.run.name,
                       import.data$fishery.scalars) == FALSE) {
      error.found <- TRUE
    } 
  }  
}

if (!is.null(import.data$target.escapement))  {
  if (exists("validate.stocks") == FALSE || validate.stocks == TRUE) {
    if (ValidTargetEscapement(person.name,
                              fram.db.conn,
                              fram.run.name,
                              import.data$target.escapement) == FALSE) {
      error.found <- TRUE
    } 
  }
}

if (error.found) {
  odbcClose(fram.db.conn)
  stop("Issues with the post season import file must be fixed before being imported")
} else {
  if (!is.null(import.data$fishery.scalars)) {
    UpdateFisheryScalars(fram.db.conn, fram.run.id, import.data$fishery.scalars)
  }
  if (!is.null(import.data$target.escapement))  {
    UpdateTargetEscapement(fram.db.conn, fram.run.id, import.data$target.escapement)
  }
}

cat("\n\n :-)  Data was successfully imported.\n")
odbcClose(fram.db.conn)




