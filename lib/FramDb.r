#'
#'
#' Common methods and constants for dealing with a FRAM Database
#'
#' @author Nicholas Komick
#' @author nicholas.komick@dfo-mpo.gc.ca
#' @section STYLE
#' Using Style (migrating too): http://style.tidyverse.org/
#'

FisheryMortSqlFilename <- "./sql/FisheryMortalities.sql"
kTotalFisheryMortSqlFilename <- "./sql/TotalFisheryMortalities.sql"
kEscapementSqlFilename <- "./sql/TotalEscapement.sql"
kFramStockSqlFilename <- "./sql/FramStocks.sql"
kFramFisherySqlFilename <- "./sql/FramFisheries.sql"
kFramRunInfoSqlFilename <- "./sql/RunInfo.sql"
kFramRunTableSqlFilename <- "./sql/RunTable.sql"
kFramGetFisheryScalars <- "./sql/GetFramFisheryScalars.sql"
kFramGetRunBaseFisheries <- "./sql/GetFramRunBaseFisheries.sql"
FramGetRunBaseStocks <- "./sql/GetFramRunBaseStocks.sql"

#' Recruit Scalar FRAM SQL scripts  ---------------------------------------------------------
FramGetSingleRecruitScalar <- "./sql/GetFramSingleRecruitScalar.sql"
kFramUpdateFisheryScalars <- "./sql/UpdateFramFisheryScalars.sql"
kFramGetStockRecSqlFilename <- "./sql/GetFramStockRecruitScalars.sql"
FramUpdateRecruitScalars <- "./sql/UpdateFramStockRecruitScalars.sql"

#' Non-Retention FRAM SQL scripts  ---------------------------------------------------------
kFramGetSingleNonRetention <- "./sql/GetFramSingleNonRetention.sql"
kFramUpdateNonRetention <- "./sql/UpdateFramNonRetention.sql"
kFramInsertNonRetention <- "./sql/InsertFramNonRetention.sql"
kFramDeleteNonRetention <- "./sql/DeleteFramNonRetention.sql"

#' Backward FRAM SQL scripts  ---------------------------------------------------------
kFramBackwardEscSqlFilename <- "./sql/FramBackwardEscapement.sql"
FramUpdateBackwardEsc <- "./sql/UpdateFramBackwardEsc.sql"
FramInsertBackwardEsc <- "./sql/InsertFramBackwardEsc.sql"
FramDeleteBackwardEsc <- "./sql/DeleteFramBackwardEsc.sql"
FramGetSingleBackwardEsc <- "./sql/GetFramSingleBackwardEscapement.sql"

kCohoSpeciesName <- "COHO"

kFramNonSelectiveScalarFlag <- 1L
kFramNonSelectiveQuotaFlag <- 2L
kFramMsfScalarFlag <- 7L
kFramMsfQuotaFlag <- 8L

FramTargetNotUsedFlag <- 0L
FramTargetEscExactFlag <- 1L
FramTargetEscSplitFlag <- 2L
FramRecruitScalarOverwriteFlag <- 9L



TranslateDbColumnNames <- function(data) {
  names(data)<- gsub("_", ".", names(data)) 
  return (data)
}

#' Check if the FRAM database as the new Comments fields for importing, if not add
#' the columns to the appropriate tables
#'
#' @param fram_db_conn An ODBC connection to the FRAM MS Access database
#'
#' @section EXCEPTIONS
#'   If the Comment column doesn't exist in the FisheryScalers or BackwardsFRAM tables
#'   an exception is failing
#' 
CheckFramCommentCol <- function(fram_db_conn) {
  error <- FALSE
  if ("Comment" %notin% sqlColumns(fram_db_conn, "FisheryScalers")$COLUMN_NAME) {
    sqlQuery(fram_db_conn, "ALTER TABLE FisheryScalers ADD COLUMN Comment TEXT(255);")
  }
  
  if ("Comment" %notin% sqlColumns(fram_db_conn, "BackwardsFRAM")$COLUMN_NAME) {
    sqlQuery(fram_db_conn, "ALTER TABLE BackwardsFRAM ADD COLUMN Comment TEXT(255);")
  }
  if ("Comment" %notin% sqlColumns(fram_db_conn, "NonRetention")$COLUMN_NAME) {
    sqlQuery(fram_db_conn, "ALTER TABLE NonRetention ADD COLUMN Comment TEXT(255);")
  }
}
#' A helper function that loads an SQL script, updates the variables in the script to values provide and
#' formats the resulting data by renames columns to common R style.
#'
#' @param db.conn An ODBC connection to the ODBC database
#' @param file.name A file name that the SQL script is saved to
#' @param variables An R list of variables, variable names in the list are matched to ones with the same name in
#'       a format like %VARIABLENAME% (eg list(runid = 1) will replace %RUNID% in the SQL with 1)
#'
#' @return A data frame with query results
#'
#' @section EXCEPTIONS
#'   If a variable type is found that the function can't handle (e.g. a vector), the script
#'   will throw an exception.
#' 
RunSqlFile <- function (db.conn, file.name, variables=NA) {
  
  file.conn <- file(file.name, "r", blocking = FALSE)
  sql.text <- paste(readLines(file.conn), collapse=" ")# empty
  close(file.conn)
  
  if (is.list(variables)) {
    var.names <- names(variables)
    
    for (var.idx in 1:length(var.names)) {
      var.name <- var.names[var.idx]
      var.value <- variables[[var.name]]
      if (is.numeric(var.value)) {
        if (is.na(var.value)) {
          var.value <- "NULL"
        }
        sql.text <- gsub(paste0("%", var.name, "%"), var.value, sql.text, ignore.case=TRUE)
      } else if (is.character(var.value) || is.factor(var.value)) {
        sql.text <- gsub(paste0("%", var.name, "%"), 
          paste0("'", as.character(var.value), "'"), 
          sql.text, 
          ignore.case=TRUE)
      } else {
        stop(sprintf("Unknown variable type '%s' for variable '%s' when converting in RunSqlFile", typeof(var.value), var.name))
      }
    }
  }
  
  unbound.variables <- gregexpr("%[a-z]*%", sql.text, ignore.case=TRUE)
  if (unbound.variables[[1]] > 0) {
    error.msg <- sprintf("Unbound variables found for the '%s' sql script \n", file.name)
    stop(error.msg)
  }
  
  data <- sqlQuery(db.conn, sql.text)
  data <- TranslateDbColumnNames(data)
  return (data)   
}

GetFramRunTable <- function (fram.db.conn, species.name) {
  # Retrieve all the FRAM runs with a run year.
  #
  # Args:
  #   fram.db.conn: An odbc connection to the FRAM database
  #   species.name: Limit to a specific species
  #
  # Returns:
  #   FRAM runs that have a run year specified in a dataframe
  #
  # Exceptions:
  #   None
  #    
  variables <- list(speciesname=species.name)
  data <- RunSqlFile(fram.db.conn, kFramRunTableSqlFilename, variables)
  return (data)
}

#' Retrieve the details about a specific FRAM run, by run name 
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param fram.run.name The FRAM run name that details are requested for
#'
#' @return FRAM run details in a data frame
#'
GetFramRunInfo <- function (fram.db.conn, fram.run.name) {
  variables <- list(runname=fram.run.name)
  data <- RunSqlFile(fram.db.conn, kFramRunInfoSqlFilename, variables)
  return (data)
}

#' A helper function loading the list of FRAM stocks 
#'
#' @param fram.db.conn An odbc connection to the FRAM database
#'
#' @return A data fram of FRAM stocks
#'
GetFramStocks <- function (fram.db.conn) {
  
  data <- RunSqlFile(fram.db.conn, kFramStockSqlFilename)
  return (data)
}

#' A helper function loading the list of FRAM fisheries in the database
#'
#' @param fram.db.conn An odbc connection to the FRAM database
#'
#' @return A data frame of FRAM fisheries
#'   
GetFramFisheries <- function (fram.db.conn) {
  data <- RunSqlFile(fram.db.conn, kFramFisherySqlFilename)
  return (data)
}

#' Get the dataframe of fishery scalars used to parameterize model runs
#'
#' @param fram.db.conn An odbc connection to the FRAM database
#' @param run.name The name of the model run you would like to retrive fishery scalars from
#'
#' @return A dataframe with the fishery scalars for a specific model run name
#'
GetFramFisheryScalars <- function (fram.db.conn, fram.run.name) {
  variables <- list(runname=fram.run.name)
  data <- RunSqlFile(fram.db.conn, kFramGetFisheryScalars, variables)
  return (data)
}

#' Get the data frame of stock recruit scalars for particular model runs
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param fram.run.name The name of the model run you would like to retrieve fisheries and timesteps from
#'
#' @return A data frame with the stock recruit scalars for a specific model run name
#'
GetFramStockRecruitScalars <- function (fram.db.conn, fram.run.name) {
  variables <- list(runname=fram.run.name)
  data <- RunSqlFile(fram.db.conn, kFramGetStockRecSqlFilename, variables)
  return (data)
}

#' Get the dataframe of valid fisheries and time steps from the base period of a specific model run
#'
#' @param fram.db.conn An odbc connection to the FRAM database
#' @param fram.run.name The name of the model run you would like to retrieve fisheries and timesteps from
#'
#' @return A dataframe with the fishery scalars for a specific model run name
#'
GetFramBaseFisheries <- function (fram.db.conn, fram.run.name) {
  
  variables <- list(runname=fram.run.name)
  data <- RunSqlFile(fram.db.conn, kFramGetRunBaseFisheries, variables)
  return (data)
}

#' Get the data frame of valid stocks from the base period of a specific model run
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param fram.run.name The name of the model run you would like to retrieve fisheries and timesteps from
#'
#' @return A dataframe with the fishery scalars for a specific model run name
#'
GetFramBaseStocks <- function (fram_db_conn, fram_run_name) {
  
  variables <- list(runname=fram_run_name)
  data <- RunSqlFile(fram_db_conn, FramGetRunBaseStocks, variables)
  return (data)
}


#' Update the fishery scalars and non retention values for an identified model run based on 
#' values in a dataframe.  The Non-Retention CNR mortalities updates more intellegently (e.g.
#' remove/adding/updating DB rows based on values provided and values within the database run)
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param fram.run.id The ID of the FRAM model run to update fishery scalars for
#' @param fishery.scalars The name of the model run you would like to retrive fishery scalars from
#'
#'
UpdateFisheryScalars <- function (fram.db.conn, fram.run.id, fishery.scalars) {
  
  
  fishery.scalars$comment.catch[is.na(fishery.scalars$comment.catch)] <- ""
  fishery.scalars$comment.cnr[is.na(fishery.scalars$comment.cnr)] <- ""
  
  for (row.idx in 1:nrow(fishery.scalars)) {
    
    variables <- list(runid = fram.run.id,
      fisheryid = fishery.scalars$fram.fishery.id[row.idx],
      timestep = fishery.scalars$fram.time.step[row.idx],
      fisheryflag = fishery.scalars$fishery.flag[row.idx],
      nonselectivecatch = fishery.scalars$nonselective.catch[row.idx],
      msfcatch = fishery.scalars$msf.catch[row.idx],
      markreleaserate = fishery.scalars$mark.release.rate[row.idx],
      markmisidrate = fishery.scalars$mark.missid.rate[row.idx],
      unmarkmissidrate = fishery.scalars$unmark.missid.rate[row.idx],
      markincidentalrate = fishery.scalars$mark.incidental.rate[row.idx],
      comment=fishery.scalars$comment.catch[row.idx])
    
    data <- RunSqlFile(fram.db.conn, kFramUpdateFisheryScalars, variables)
    
    
    cnr.mortalities <- as.numeric(fishery.scalars$cnr.mortalities[row.idx])
    
    variables <- list(runid = fram.run.id,
      fisheryid = fishery.scalars$fram.fishery.id[row.idx],
      timestep = fishery.scalars$fram.time.step[row.idx])
    
    nonret.data <- RunSqlFile(fram.db.conn, kFramGetSingleNonRetention, variables)
    
    if (is.na(cnr.mortalities)) {
      if (nrow(nonret.data) > 0) {
        #remove the CNR Mortality entry
        variables <- list(runid = fram.run.id,
          fisheryid = fishery.scalars$fram.fishery.id[row.idx],
          timestep = fishery.scalars$fram.time.step[row.idx])
        
        data <- RunSqlFile(fram.db.conn, kFramDeleteNonRetention, variables)       
      } else {
        #no data provided and no data in DB, so nothing to do.
      }
    } else {
      variables <- list(runid = fram.run.id,
        fisheryid = fishery.scalars$fram.fishery.id[row.idx],
        timestep = fishery.scalars$fram.time.step[row.idx],
        cnrmortalities = cnr.mortalities,
        comment = fishery.scalars$comment.cnr[row.idx])
      if (nrow(nonret.data) > 0){
        
        if (cnr.mortalities != nonret.data$cnr.mortalities) {
          #Updating the CNR value becaues it has changed
          data <- RunSqlFile(fram.db.conn, kFramUpdateNonRetention, variables)     
        } else {
          #Value hasn't changed so do nothing.
        }
      } else {
        #Insert a new NonRetention row into the database.
        data <- RunSqlFile(fram.db.conn, kFramInsertNonRetention, variables)        
      }
    }
  }
  return ()
}


#' Update the stock recruit scalars and the backward FRAM target escapement values/flags.
#' The target escapement updates more intellegently (e.g. remove/adding/updating DB rows based on 
#' flag and values provided and values within the database run)
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param fram.run.id The ID of the FRAM model run to update fishery scalars for
#' @param fishery.scalars The name of the model run you would like to retrive fishery scalars from
#'
UpdateTargetEscapement <- function (fram_db_conn, fram_run_id, escapement_df) {
  
  for (row_idx in 1:nrow(escapement_df)) {
    
    
    
    variables <- list(runid = fram.run.id,
      stockid = escapement_df$fram.stock.id[row_idx])
    db_recruit <- RunSqlFile(fram.db.conn, FramGetSingleRecruitScalar, variables)
    
    if(nrow(db_recruit) == 0) {
      db_recruit <- data.frame(recruit.scalar = as.numeric(NA))
    }
    
    # if ((escapement_df$pair_esc_flag[row_idx] == FramRecruitScalarOverwriteFlag) && #MOBJOB
    #     (db_recruit$recruit.scalar != escapement_df$recruit.scalar[row_idx])){
    #   
    #   variables <- list(runid = fram.run.id,
    #     stockid = escapement_df$fram.stock.id[row_idx],
    #     recruitscalar = escapement_df$recruit.scalar[row_idx])
    #   
    #   data <- RunSqlFile(fram.db.conn, FramUpdateRecruitScalars, variables)
    #   
    # } else {
    
    ##OLD MOB Start from sractch
    
    # if (escapement_df$escapement.flag[row_idx] != FramRecruitScalarOverwriteFlag) {
    #   if ((is.na(db_recruit$recruit.scalar) && escapement_df$recruit.scalar[row_idx] != 0) ||
    #       (coalesce(db_recruit$recruit.scalar, 0) != escapement_df$recruit.scalar[row_idx])) {
    #     cat(sprintf("WARNING - %s mismatched/ updated recruit scalar not imported because of escapement flag (%f -> %f).\n",
    #       escapement_df$fram.stock.name[row_idx],
    #       db_recruit$recruit.scalar,
    #       escapement_df$recruit.scalar[row_idx]))
    #   }
    # } else {
    #   if (is.na(db_recruit$recruit.scalar)) {
    #     if (escapement_df$recruit.scalar[row_idx] == 0) {
    #       cat(sprintf())
    #     } else {
    #       cat(sprintf("ERROR - '%s' recruit scalar not defined in the database, but a value has been provided in import (%f).\n",
    #         escapement_df$fram.stock.name,
    #         escapement_df$recruit.scalar))
    #       stop("The importer does not know how to deal with this situation.")
    #     } 
    #   } else if (db_recruit$recruit.scalar != escapement_df$recruit.scalar[row_idx]) {
    #     variables <- list(runid = fram.run.id,
    #       stockid = escapement_df$fram.stock.id[row_idx],
    #       recruitscalar = escapement_df$recruit.scalar[row_idx])
    #     
    #     data <- RunSqlFile(fram.db.conn, FramUpdateRecruitScalars, variables)
    #   }
    # }
    # }
    
    if (escapement_df$escapement.flag[row_idx] == FramRecruitScalarOverwriteFlag) 
    {
      
      RS.error <- FALSE
      
      if(escapement_df$recruit.scalar[row_idx] == 0){
        cat(sprintf("ERROR - '%s' recruit scalar set to update but recruit scalar set to 0",
          escapement_df$fram.stock.name[row_idx]))
      }
      
      if(is.na(db_recruit$recruit.scalar)) {
        cat(sprintf("ERROR - '%s' recruit scalar not defined in the database, but a value has been provided in import (%f).\n\n",
          escapement_df$fram.stock.name[row_idx],
          escapement_df$recruit.scalar[row_idx]))
        RS.error <- TRUE
      }
      
      if(escapement_df$target.escapement[row_idx] > 0){
        cat(sprintf("ERROR - Recruit scalar update flag set but target escapement value provided - please change flag for '%s'\n\n",
          escapement_df$fram.stock.name[row_idx]))
        RS.error <- TRUE
      }
      
      if(RS.error == TRUE){
        stop("error with recruit scalar flagging \n")
      }else{
        
        if(escapement_df$recruit.scalar[row_idx] ==  db_recruit$recruit.scalar){
          cat(sprintf("WARNING - Requested recruit scalar update for '%s' but provided and db recruit sclars are the same.\n\n",
            escapement_df$fram.stock.name[row_idx]))
        }
        
        if(escapement_df$recruit.scalar[row_idx] !=  db_recruit$recruit.scalar & !is.na(db_recruit$recruit.scalar)){
          
          cat(sprintf("INFO - Recruit scalar being updated for '%s' \n\n",
            escapement_df$fram.stock.name[row_idx]))
          
          variables <- list(runid = fram.run.id,
            stockid = escapement_df$fram.stock.id[row_idx],
            recruitscalar = escapement_df$recruit.scalar[row_idx])
          
          data <- RunSqlFile(fram.db.conn, FramUpdateRecruitScalars, variables)
        }
      }
    }
    
    if(!is.na(db_recruit) & escapement_df$escapement.flag[row_idx] == FramTargetNotUsedFlag &  escapement_df$pair_esc_flag[row_idx] != 2){
      cat(sprintf("Error - stock '%s' requires flag \n\n",  escapement_df$fram.stock.name[row_idx]))
      stop("No flag provided on escapement stock")
    }
    
    esc.flag <- as.numeric(escapement_df$escapement.flag[row_idx]) 
    if (esc.flag == FramRecruitScalarOverwriteFlag){
      esc.flag <- FramTargetNotUsedFlag
    }
    
    
    target.escapement <- as.numeric(escapement_df$target.escapement[row_idx])
    if (is.na(target.escapement)) {
      target.escapement <- 0
    }
    
    comment <- escapement_df$comment[row_idx]
    if (is.na(comment)) {
      comment <- ""
    }
    
    variables <- list(runid = fram.run.id,
      stockid = escapement_df$fram.stock.id[row_idx])
    
    esc.data <- RunSqlFile(fram.db.conn, FramGetSingleBackwardEsc, variables)
    
    
    variables <- list(runid = fram.run.id,
      stockid = escapement_df$fram.stock.id[row_idx],
      escapementflag = esc.flag,
      targetescapement = target.escapement,
      comment=comment)
    if (nrow(esc.data) > 0) {
      data <- RunSqlFile(fram.db.conn, FramUpdateBackwardEsc, variables)
    } else {
      #Insert a new Backward FRAM Escapement Target row into the database.
      data <- RunSqlFile(fram.db.conn, FramInsertBackwardEsc, variables)        
    }
  }
  
  return ()
}

#' A helper function loading the total mortalities for all fisheries and time steps within a FRAM model run 
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param run.name The name of the model run you would like to load fishery mortalities for
#'
#' @return A dataframe with the mortalities from the FRAM fisheries and time steps for a specific model run name
#'
#' @section Exceptions:
#'   The method checks the run year of the model run against a provided value, if they don't match 
#'   then the method throws an exception.
#'  
GetFramFisheryMortality <- function (fram.db.conn, run.name, run.year) {
  
  variables <- list(runname=run.name)
  data <- RunSqlFile(fram.db.conn, FisheryMortSqlFilename, variables)
  
  data.run.year <- unique(data$run.year)
  if (all(is.na(data$run.year))) {
    cat(sprintf("WARNING: Run name '%s' has no run year set for fishery mortality, so assume run year %d\n", run.name, run.year))
    data$run.year <- run.year
  } else if (any(data.run.year %notin% run.year)) {
    stop(sprintf("Run name '%s' has a run year that doesn't match the specified", run.name))
  }
  return (data)
}

GetFramTotalFisheryMortality <- function (fram.db.conn, run.name, run.year) {
  # A helper function loading the total mortalities for all fisheries within a FRAM model run 
  #
  # Args:
  #   fram.db.conn: An odbc connection to the FRAM database
  #   run.name: The name of the model run you would like to load fishery mortalities for
  #
  # Returns:
  #   A dataframe with the mortalities from the FRAM fisheries for a specific model run name
  #
  # Exceptions:
  #   The method checks the run year of the model run against a provided value, if they don't match 
  #   then the method throws an exception.
  #   
  variables <- list(runname=run.name)
  data <- RunSqlFile(fram.db.conn, kTotalFisheryMortSqlFilename, variables)
  
  data.run.year <- unique(data$run.year)
  if (all(is.na(data$run.year))) {
    cat(sprintf("WARNING: Run name '%s' has no run year set for fishery mortality, so assume run year %d\n", run.name, run.year))
    data$run.year <- run.year
  } else if (any(data.run.year %notin% run.year)) {
    stop(sprintf("Run name '%s' has a run year that doesn't match the specified", run.name))
  }
  return (data)
}

#' A helper function loading the stock specific escapement from a FRAM model run 
#'
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param run.name The name of the model run you would like to load fishery mortalities for
#' @param run.year The run year for the run name, used as a cross check when loading the data
#'
#' @return A dataframe with the mortalities from the FRAM fisheries for a specific model run name
#'
#' @section Exceptions:
#'   The method checks the run year of the model run against a provided value, if they don't match 
#'   then the method throws an exception.
#'  
GetFramTotalEscapement <- function (fram.db.conn, run.name, run.year) {
  variables <- list(runname=run.name)
  data <- RunSqlFile(fram.db.conn, kEscapementSqlFilename, variables)
  
  data.run.year <- unique(data$run.year)
  if (all(is.na(data$run.year))) {
    cat(sprintf("WARNING: Run name '%s' has no run year set for escapement, so assume run year %d\n", run.name, run.year))
    data$run.year <- run.year
  } else if (any(data.run.year %notin% run.year)) {
    stop(sprintf("Run name '%s' has a run year that doesn't match the specified", run.name))
  }
  
  return (data)
}

#' A helper function retrieving the escapement values used by the backward FRAM during post-season run 
#' 
#' @param fram.db.conn An ODBC connection to the FRAM database
#' @param run.name The name of the model run you would like to retrieve backward FRAM Escapement values for
#'
#' @return A data frame with the Backward FRAM escapement data, based on the model run name provided
#'  
GetFramBackwardEscapement <- function (fram.db.conn, fram.run.name) {
  variables <- list(runname=fram.run.name)
  data <- RunSqlFile(fram.db.conn, kFramBackwardEscSqlFilename, variables)
  return (data)
}


#' A function that loads the PSC stock and fishery reference tables from CSV files.  
#' The resulting tables are combined into a list
#'
#' @param data.dir The directory where there reference csv files are saved
#'
#' @return A list with the psc.stock, psc.stock.map, psc.fishery, and psc.fishery.map dataframes
#'
#' @section Exceptions:
#'   If any of the expected CSV files do not exist, a error is thrown.
#'    
LoadPscData <- function(data.dir) {
  
  
  psc.fishery <- ReadCsv("PSCFisheries.csv", data.dir, unique.col.names=c("psc.fishery.id"))
  psc.fishery.map <- ReadCsv("PSCFisheryMap.csv", data.dir, unique.col.names=c("fram.fishery.id"))
  psc.stock <- ReadCsv("PSCStocks.csv", data.dir, unique.col.names=c("psc.stock.id"))
  psc.stock.map <- ReadCsv("PSCStockMap.csv", data.dir, unique.col.names=c("fram.stock.id"))    
  
  result.list <- list(psc.fishery = psc.fishery,
    psc.fishery.map = psc.fishery.map,
    psc.stock = psc.stock,
    psc.stock.map = psc.stock.map)
  
  return (result.list)
}
