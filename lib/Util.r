################
#
# General Utility Functions for R scripts for FRAM Admin
#
# Nicholas Komick
# nicholas.komick@dfo-mpo.gc.ca
# May 22, 2014
# Coding Style: http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html
#
#
################

###____ Constants Section ____####

#Generic constants used in other scripts
kNAChar <- as.character(NA)
kNANumber <- as.numeric(NA)
kUnspecified <- "Unspecified"

#Define %notin% as the inverse of %in%
"%notin%" <- Negate("%in%")

#The next vector provides a list of previous R files that were loaded.
#This allows the LoadSourceFile function to skip the file if it was
#previously loaded
prev.loaded.src.files <- c()

#' Loads required packages.  If they are not installed, then
#' they are automatically installed.
#'
#' @param required.packages A vector of required package names
#'
#'
InstallRequiredPackages <- function (required.packages) {
  new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
    install.packages(new.packages, dependencies=TRUE, repos = "http://cran.us.r-project.org")
  }
  
  for (package.name in required.packages) {
    suppressPackageStartupMessages(library(package.name, character.only = TRUE))
  }
}

InstallRequiredPackages(c("readr"))

#' Provides a standardized text based time stamp for inclusion in file names
#'
#' @return A string with the current date and time
#'
GetTimeStampText <- function() {
  current.time <- Sys.time()
  return (format(current.time, "%Y%m%d_%H%M%S"))
}

#' Loads an R file.  This function makes sure a given source file is accidently loaded multiple times
#'
#' @param source.file.name The file name of the R source to be loaded
#'
#' @return A string with the current date and time
#'
LoadSourceFile <- function (source.file.name) {
  source.fname.full <- normalizePath(source.file.name)
  if (source.fname.full %notin% prev.loaded.src.files) {
    source(source.fname.full)
  } else {
    cat(sprintf("Skip loading '%s', previously loaded.", source.file.name), sep = "\n")
  }
}

#' Convert text into title case
#' Got this regex from: http://stackoverflow.com/questions/15776732/how-to-convert-a-vector-of-strings-to-title-case
#' This function could use \\E (stop capitalization) rather than \\L (start lowercase), depending on what rules you want to follow
#'
#' @param text A vector of strings that are to be converted to title case
#'
#' @return The provided text in title case
#'
TitleCase <- function(text) {
  return (gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2" ,text, perl=TRUE))
}

FormatInt <- function(values) {
  # Formats a value to an integer in text form.
  # This is typically used to generate pretty report text values
  #
  # Args:
  #   values: A vector of values to convert to Integer format. Can be factor, character, or number
  #
  # Returns:
  #   A vector of text values in integer format (i.e. number with no decimal place).
  #
  # Exceptions:
  #   Fails if the provided value is of a type unknown to the function (e.g. not a factor, character, or number)
  #
  fmt.values <- rep("", length(values))

  #Don't combine with else, these if statements should cascade without elses connecting them.
  if (is.factor(values)) {
    values <- as.character(values)
  }
  if (is.character(values)) {
    values <- as.numeric(values)
  }

  if (is.numeric(values)) {
    has.value <- !is.na(values)
    round.value <- round(values[has.value])
    fmt.values[has.value] <- formatC(round.value, format="d", big.mark=',')
  } else {
    stop("Unknown type for values when calling FormatInt")
  }

  return (fmt.values)
}

FormatDouble <- function(value, decimals) {
  # Formats a value to an double in text form with the specified number of decimal places.
  # This is typically used to generate pretty report text values
  #
  # Args:
  #   value: A vector of values to convert to Double format. Can be factor, character, or number
  #   decimals: how many decimal places the final values should display
  #
  # Returns:
  #   A vector of text values in integer format (i.e. number with no decimal place).
  #
  # Exceptions:
  #   Fails if the provided value is of a type unknown to the function (e.g. not a factor, character, or number)
  #
  fmt.value <- rep("", length(value))
  #Don't combine with else, these if statements should cascade without elses connecting them.
  if (is.factor(value)) {
    value <- as.character(value)
  }
  if (is.character(value)) {
    value <- as.numeric(value)
  }

  if (is.numeric(value)) {
    has.value <- !is.na(value)
    round.value <- round(value[has.value], decimals)
    if (round.value == 0) {
      fmt.value[has.value] <- "0.0"
    } else {
      fmt.value[has.value] <- formatC(round.value, digits=1, format="f")
    }

  } else {
    stop("Unknown type for values when calling FormatInt")
  }

  return (fmt.value)
}


WriteCsv <- function(file, data) {
	# A helper function for writing CSV files.
	# This is a simple wrapper to handle default flags in other script files
	#
	# Args:
	#   file: File name or file connection to write CSV file to
	#   data: Data frame to be written to CSV file
	#
	# Returns:
	#   None
	#
	# Exceptions:
	#   None
	#
  write.csv(data, file=file, row.names=FALSE, na="")
}

#' A helper function for writing CSV file format text into R vector of characters
#'
#' @param x A data frame to be written to as CSV format to a vector of characters
#'
#' @result A character vector with the lines of CSV
#'
WriteMemoryCsv <- function(x) {
  text.con <- textConnection("text.data", "w")
  write.csv(x, file=text.con, row.names=FALSE, na="")
  close(text.con)
  return (text.data)
}


WriteProtectedCsv <- function (data, file.name) {
	# A helper function for writing CSV files and setting them to readonly for protection.
	# When writing the file, the file is first set to writable then written over then set to readonly
	#
	# Args:
	#   data: Data frame to be written to CSV file
	#   file.name: File name to write CSV file to
	#
	# Returns:
	#   None
	#
	# Exceptions:
	#   None
	#

	Sys.chmod(file.name, mode = "0777", use_umask=TRUE)
	write.csv(data, file=file.name, row.names=FALSE)
	#Make the file read-only to prevent accidental modification/deletion
	Sys.chmod(file.name, mode = "0444", use_umask=TRUE)
}

#' Read a CSV file from a predefined data directory
#' Optionally, the function can check the file for uniqueness by providing a vector
#' of column names that should be unique to each row.
#' 
#' This function always assumes that the CSV file has a header row.
#' 
#' @param file.name The file name of the CSV file to load
#' @param data.dir The directory that the CSV file is saved
#' @param unique.col.names (optional): A vector of column names that combined uniquely identify each row
#'
ReadCsv <- function (file.name, data.dir, unique.col.names = NULL) {
  if (!is.na(data.dir) & nchar(data.dir) > 0) {
    file.name <- file.path(data.dir,file.name)
  }
  
  data <- suppressMessages(read_csv(file=file.name, 
                                   col_names=TRUE))

  if (!is.null(unique.col.names)) {
    col.names <- names(data)
    invalid.col.names <- col.names[unique.col.names %notin% col.names]
    if (length(invalid.col.names) > 0) {
      error.msg <- paste0("The '%s' file does not contain the following column names for unique checking: ", paste(invalid.col.names, collapse=","))
      stop(error.msg)
    }

    uniq.data <- data[, names(data) %in% unique.col.names]

    if (is.data.frame(uniq.data)) {
      if (nrow(unique(uniq.data)) != nrow(uniq.data)) {
        error.msg <- sprintf("The '%s' file as non-unique data based on the following columns: \n%s\n", file.name, paste(unique.col.names, collapse=","))
        stop(error.msg)
      }
    } else {
      if (length(unique(uniq.data)) != length(uniq.data)) {
        error.msg <- sprintf("The '%s' file as non-unique data based on the following columns: \n%s\n", file.name, paste(unique.col.names, collapse=","))
        stop(error.msg)
      }
    }
  }
  return (data)
}

#' A helper function for reading CSV formated text from a R character variable
#'
#' @param x A data frame to be written to as CSV format to a vector of characters
#'
#' @result A character vector with the lines of CSV
#'
ReadMemoryCsv <- function(x) {
  x.df <- read_csv(x)
  return(x.df)
}

ValidateValueDomain <- function (values, domain, error.message="The following values are invalid:\n\n%s\n\n") {
	# A helper function for validating data within a pre-defined set of possible values.
	#
	# Args:
	#   values: The data that is to be validated
	#   domain: The set of valid values that "values" can take on.
	#   error.message: A template of a printf message for invalid values.
	#
	# Returns:
	#   None
	#
	# Exceptions:
	#   If the values vector contains values that are not in the domain vector, then the method calls stop
	#   with an error message using the template provide in the "error.message" parameter.

	invalid.values <- values %notin% domain
	if (any(invalid.values)) {
		invalid.values <- values[invalid.values]
		invalid.values <- unique(invalid.values)
		stop(sprintf(error.message, paste(invalid.values, collapse=", ")))
	}
}

#' Loads a R source file that is used for configuration
#'
#' @param report.config.file The file name that is used to load configuration parameters
#'
LoadConfigFiles <- function (report.config.file) {

	current.dir <- normalizePath(getwd())

	if (is.na(report.config.file) == FALSE) {
	  if (!file.exists(report.config.file)) {
	    stop(sprintf("Unvalid configuration file name provided: %s", report.config.file))
	  }
		source(report.config.file)
	}
}

#' Formats a vector of strings to a single text block.
#' If more than "show_total" lines, the text block is trucated with text
#' identifying that there is more not shown.
#'
#' @param text_lines A vector of text lines to format
#' @param show_total How many lines of text_lines to show in the output
#' @param sep Character used to seperate the lines
#' 
#' @return A single text block.
#'
FormatTruncTextSet <- function(text_lines, show_total=5, sep="\n") {
  end_line <- "\n"
  line_total <- length(text_lines)
  if (line_total > show_total) {
    end_line <- sprintf("... +%d more ...\n", line_total - show_total)
  }
  
  show_total <- min(line_total, show_total)
  
  return(paste0(c(text_lines[1:show_total], end_line), collapse=sep))
}

#' Formats parallel vectors of names and IDs a single text block.
#' This is mainly used to identify fisheries or stocks with a particular issue.
#'
#' @param names A vector of names to format
#' @param ids  A vector of ids to format
#' @param show_total How many lines of text_lines to show in the output
#' @param sep Character used to seperate the lines
#' 
#' @return A single text block.
#'
FormatNameIdText <- function(names, ids, show_total=5, sep="\n") {
  
  text <- paste0(names,
                 " (",
                 ids,
                 ")")
  
  return (FormatTruncTextSet(text, show_total, sep))
}




