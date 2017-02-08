################
#
# Common methods to deal with a TAMM Spreadsheet Model
#
# Nicholas Komick
# nicholas.komick@dfo-mpo.gc.ca
# January 12, 2017
# Using: http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html
#
################

kDecimalRegEx <- "^[0-9]*[.]?[0-9]+([eE][-+]?[0-9]+)?$"

required.packages <- c("dplyr", "readxl")
InstallRequiredPackages(required.packages)

GetTammValues <- function (tamm.file.name, tamm.ref) {
  # A helper function loading the total mortalities from the TAMM excel spreadsheet
  #
  # Args:
  #   tamm.file.name: An odbc connection to the FRAM database
  #
  # Returns:
  #   A dataframe with the FRAM fisheries and associated TAMM mortalties
  #
  # Exceptions:
  #   The method checks that values read for the Excel spreadsheet are numeric values.
  #   
  
  tamm.ref <- arrange(tamm.ref, tamm.worksheet.name)
  tamm.ref$tamm.value <- as.character(NA)
  
  prev.worksheet.name <- ""
  
  worksheet.data <- NA
  
  for(ref.idx in 1:nrow(tamm.ref)) {
    
    if (tamm.ref$tamm.worksheet.name[ref.idx] != prev.worksheet.name) {
      worksheet.data <- read_excel(tamm.file.name, 
                                   tamm.ref$tamm.worksheet.name[ref.idx], 
                                   col_names = FALSE)
      
      cat(sprintf("Loading data from excel worksheet: %s\n", 
                  tamm.ref$tamm.worksheet.name[ref.idx]))
      
      prev.worksheet.name <- tamm.ref$tamm.worksheet.name[ref.idx]
      
    }
    
    tamm.ref$tamm.value[ref.idx] <- worksheet.data[tamm.ref$tamm.cell.row[ref.idx],
                                                   tamm.ref$tamm.cell.col[ref.idx]][[1]]
  }
  
  decimal.check <- grepl(kDecimalRegEx, tamm.ref$tamm.value)
  
  if (any(!decimal.check)) {
    
    cat("The following TAMM References do no return decimal values:\n\n")
    cat(paste(names(tamm.ref), collapse=","))
    cat("\n")
    
    cat(paste(tamm.ref[!decimal.check,], collapse=",", sep="\n"))
    cat("\n")
    
    stop("A TAMM cell reference is not returing a decimal value, this must be fixed continue generating the report.")
  }
  
  tamm.ref$tamm.value <- as.numeric(tamm.ref$tamm.value)
  return(tamm.ref)
}

GetTammFisheryMortality <- function (tamm.file.name, data.dir) {
  # Reads the specific Stock/Fishery mortality values from a TAMM model defined in the 
  # "./data/TammFisheryRef.csv" file.
  #
  # Args:
  #   tamm.file.name: The file name of TAMM excel spreadsheet
  #   data.dir: Directory that TAMM reference files are defined
  #
  # Returns:
  #   A dataframe with the FRAM fisheries/stock combination and associated TAMM mortalties
  #
  # Exceptions:
  #   The method checks that values read for the Excel spreadsheet are numeric values.
  #   
  
  
  tamm.fishery.ref <- ReadCsv("TammFisheryRef.csv", data.dir, unique.col.names=c("fram.stock.id", "fram.fishery.id"))
  tamm.fishery.ref <- GetTammValues(tamm.file.name, tamm.fishery.ref)
  tamm.fishery.ref <- select(tamm.fishery.ref,fram.stock.id, fram.fishery.id, tamm.value)
  
  return (tamm.fishery.ref)
}


GetTammEscapement <- function (tamm.file.name, data.dir) {
  # Reads the specific Escapement values from a TAMM model defined in the 
  # "./data/TammEscRef.csv" file.
  #
  # Args:
  #   tamm.file.name: The file name of TAMM excel spreadsheet
  #   data.dir: Directory that TAMM reference files are defined
  #
  # Returns:
  #   A dataframe with the FRAM stock ID and associated TAMM escapement
  #
  # Exceptions:
  #   The method checks that values read for the Excel spreadsheet are numeric values.
  #  
  
  tamm.esc.ref <- ReadCsv("TammEscRef.csv", data.dir, unique.col.names=c("fram.stock.id"))
  tamm.esc.ref <- GetTammValues(tamm.file.name, tamm.esc.ref)
  tamm.esc.ref <- select(tamm.esc.ref,fram.stock.id, tamm.value)
  
  return (tamm.esc.ref)
}

GetTammData <- function (tamm.file.name, data.dir) {
  # Reads the various values of the TAMM spreadsheet and packages data into a list
  #
  # Args:
  #   tamm.file.name: The file name of TAMM excel spreadsheet
  #   data.dir: Directory that TAMM reference files are defined
  #
  # Returns:
  #   A list with a dataframe for fishery mortalities and escapement values
  #
  # Exceptions:
  #   None
  #   
  
  result.list <- list(tamm.fishery.mortalities = GetTammFisheryMortality(tamm.file.name, data.dir),
                      tamm.escapement = GetTammEscapement(tamm.file.name, data.dir))
  
  return (result.list)
}

