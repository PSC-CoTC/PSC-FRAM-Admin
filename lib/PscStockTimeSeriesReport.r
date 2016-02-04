################
#
# Code to generate a spreadsheet with time series data for each PSC Stock
#
# Nicholas Komick
# nicholas.komick@dfo-mpo.gc.ca
# June 15, 2015
# Using: http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html
#
#
################


rm(list=ls())     	#clean up the workspace
header <- "CoTC PSC Stock Time Series Export Tool v0.1 beta"

#Default configuration file name, used if non-provided typically when debugging
def.config.file.name <- "timeseries_report_config.r"
excel.doc.template.filename <- "./template/psc_stock_timeseries.xlsx"

options(java.parameters = "-Xmx1024m")
options(stringsAsFactors = FALSE) #Leave text fields as text.

kEscapementName <- "Escapement"
kCohortName <- "Cohort"


#Setup default directories
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

LoadSourceFile(file.path(source.lib.dir, "FramDb.r"))
LoadSourceFile(file.path(source.lib.dir, "AnnualReportLib.r"))

#RODBC - Used to connect to the FRAM MS Access DB.
#XLConnect - Used to produce Excel reports
required.packages <- c("RODBC", "XLConnect", "data.table")
InstallRequiredPackages(required.packages)

GetRunYearVector <- function(run.year.range) {
  if (length(run.year.range) != 2 || is.numeric(run.year.range) == FALSE) {
    stop("Run year range parameter must be two number vector.")
  }
  
  if (run.year.range[1] > run.year.range[2]) {
    stop("For the run year range, the first year must be before the second year.")
  }
  
  return(run.year.range[1]:run.year.range[2])
}

AddStockWorksheet <- function (excel.doc, data.table, stock.name) {
  createSheet(excel.doc, stock.name)
  
  title <- data.frame(stock=stock.name)
  writeWorksheet (excel.doc, title, sheet=stock.name, header=FALSE)
  
  writeWorksheet (excel.doc, data.table, sheet=stock.name, startRow=2)
  er.style <- getCellStyle (excel.doc, "Stock Title")
  setCellStyle(excel.doc, sheet = stock.name, row = 1, col = 1, cellstyle = er.style)
  
  er.style <- getCellStyle (excel.doc, "ER Value")
  cell.coord <- expand.grid(row=2:nrow(data.table), col=1:ncol(data.table))
  setCellStyle(excel.doc, sheet = stock.name, row = cell.coord$row, col = cell.coord$col, cellstyle = er.style)
  
  setColumnWidth(excel.doc, sheet = stock.name, column = 1, width = 6000)
  setColumnWidth(excel.doc, sheet = stock.name, column = 2:ncol(data.table), width = 2000)

  sub.total.style <- getCellStyle (excel.doc, "Sub Total")
  cell.coord <- expand.grid(row=c(17,27), col=1:ncol(data.table))
  setCellStyle(excel.doc, sheet = stock.name, row = cell.coord$row, col = cell.coord$col, cellstyle = sub.total.style)
  
  header.style <- getCellStyle (excel.doc, "Header")
  cell.coord <- expand.grid(row=c(2), col=1:ncol(data.table))
  setCellStyle(excel.doc, sheet = stock.name, row = cell.coord$row, col = cell.coord$col, cellstyle = header.style)    
  
  total.style <- getCellStyle (excel.doc, "Col Total")
  cell.coord <- expand.grid(row=c(29), col=1:ncol(data.table))
  setCellStyle(excel.doc, sheet = stock.name, row = cell.coord$row, col = cell.coord$col, cellstyle = total.style)  
  
  aux.total.style <- getCellStyle (excel.doc, "Aux Total")
  cell.coord <- expand.grid(row=c(30,31), col=1:ncol(data.table))
  setCellStyle(excel.doc, sheet = stock.name, row = cell.coord$row, col = cell.coord$col, cellstyle = aux.total.style)  
}

FormatStockTable <- function (stock.table, run.years) {
  #stock.table <- stock.tables[[1]]
  stock.table <- data.table(stock.table)
  stock.table[psc.fishery.name == kEscapementName, psc.fishery.order := 9998]
  stock.table[psc.fishery.name == kCohortName, psc.fishery.order := 9999]
  
  setkey(stock.table, psc.fishery.order)
  canada.sum <- data.table(t(colSums(stock.table[group.code == kCanadaGroupCode,as.character(run.years), with=FALSE], na.rm=TRUE)))
  canada.sum[,psc.fishery.name := kCanadaGroupCode]
  
  col.order <- c("psc.fishery.name", as.character(run.years))
  setcolorder(canada.sum, col.order)
  
  south.us.sum <- data.table(t(colSums(stock.table[group.code == kUSGroupCode,as.character(run.years), with=FALSE], na.rm=TRUE)))
  south.us.sum[,psc.fishery.name := kUSGroupCode]
  setcolorder(south.us.sum, col.order)
  
  summary.tbl <- stock.table[group.code == kCanadaGroupCode,
                             names(stock.table) %notin% c("ID", "psc.fishery.id", "psc.fishery.order", "group.code"), 
                             with=FALSE ]
  
  summary.tbl <- rbindlist(list(summary.tbl, canada.sum))
  
  south.us.rows <- stock.table[group.code == kUSGroupCode,
                               names(stock.table) %notin% c("ID", "psc.fishery.id", "psc.fishery.order", "group.code"), 
                               with=FALSE ]
  
  summary.tbl <- rbindlist(list(summary.tbl, south.us.rows, south.us.sum))
  
  
  other.rows <- stock.table[(is.na(group.code) | group.code == "") & psc.fishery.name %notin% c(kEscapementName, kCohortName) ,
                            names(stock.table) %notin% c("ID", "psc.fishery.id", "psc.fishery.order", "group.code"), 
                            with=FALSE]
  
  total.sum <- rbindlist(list(canada.sum, south.us.sum, other.rows))
  total.sum <- data.table(t(colSums(total.sum[,2:ncol(total.sum), with=FALSE], na.rm=TRUE)))
  total.sum[,psc.fishery.name := kTotalFisheryName]
  setcolorder(total.sum, col.order)
  
  final.rows <- stock.table[psc.fishery.name %in% c(kEscapementName, kCohortName) ,
                            names(stock.table) %notin% c("ID", "psc.fishery.id", "psc.fishery.order", "group.code"), 
                            with=FALSE]
  
  summary.tbl <- rbindlist(list(summary.tbl, other.rows, total.sum, final.rows))
  
  setnames(summary.tbl,"psc.fishery.name","Fishery Name")
  
  return(summary.tbl)
}

Main <- function (config.file.name) {
  fram.db.conn <- odbcConnectAccess(normalizePath(fram.db.name))
  psc.data.list <- LoadPscData(data.dir)
  
  run.year.vector <- GetRunYearVector(run.year.range)
  runs <- GetRunTable(fram.db.conn, kCohoSpeciesName)
  
  runs <- runs[runs$run.year %in% run.year.vector, ]
  
  stock.tables <- list()
  
  for (run.year in run.year.vector) {
    cat(run.year, sep="\n")
    
    run.name <- runs$run.name[runs$run.year == run.year]
    if (length(run.name) == 0) {
      cat(sprintf("Run Year Skipped - No FRAM run for run year: %d\n", run.year))
      next
    } else if (length(run.name) > 1) {
      cat(sprintf("Run Year Skipped - More then one model run for run year: %d\n", run.year))
      next
    }
    
    season.data <- CompilePscData(fram.db.conn, run.name, run.year, psc.data.list)
    fishery.mort <- data.table(season.data$fishery.mortality)
    stock.summary <- data.table(season.data$stock.summary)
    
    stock.names <- unique(stock.summary$psc.stock.name)
    
    for(stock.idx in 1:length(stock.names)) {
      curr.stock.name <- stock.names[stock.idx]
      stock.er <- fishery.mort[psc.stock.name == curr.stock.name, .(psc.fishery.name,er)]
      setnames(stock.er, "er", as.character(run.year))
      
      aux.data <- data.frame(c(kEscapementName, kCohortName),
                             t(stock.summary[psc.stock.name == curr.stock.name, .(escapement, cohort)]))
      
      stock.er <- rbindlist(list(stock.er, aux.data))
      
      setkey(stock.er, psc.fishery.name)
      
      if (stock.names[stock.idx] %in% names(stock.tables)) {
        prev.stock.er <- stock.tables[[stock.names[stock.idx]]]
        stock.er <- merge(prev.stock.er, stock.er, all=TRUE, by="psc.fishery.name")
        stock.tables[[stock.names[stock.idx]]] <- stock.er
      } else {
        stock.tables[[stock.names[stock.idx]]] <- merge(psc.data.list$psc.fishery, stock.er, by="psc.fishery.name", all=TRUE)
      }
    }
  }
  odbcClose(fram.db.conn)
  
  file.copy(excel.doc.template.filename, excel.result.doc)
  result.doc <- loadWorkbook(excel.result.doc, create = FALSE)
  
  psc.stock <- data.table(psc.data.list$psc.stock)
  setkey(psc.stock, psc.stock.order)
  
  for(stock.name in psc.stock$psc.stock.name) {
    curr.stock.tbl <- stock.tables[[stock.name]]
    curr.stock.tbl <- FormatStockTable(curr.stock.tbl, run.year.vector)
    
    AddStockWorksheet(result.doc, curr.stock.tbl, stock.name)
  }
  
  #cleanup a blank worksheet in the template, this is in template because you need at least a single workseet in doc
  removeSheet(result.doc, sheet = "deleteme")
  saveWorkbook(result.doc)
  
  cat(sprintf("The PSC Stock time series report written to:\n\n%s\n\n", normalizePath(excel.result.doc)))  
  
  return (stock.tables)
}


#############################################################
#
# Main Section
#
#  * Interpret Command Line parameters and load the configuration file.
#
#############################################################

cmdArgs <- commandArgs(TRUE)
if(length(cmdArgs) > 0) {
  print(cmdArgs)
} else {
  cat("No command line parameters provided.\n")
}

config.file.name <- cmdArgs[1]

if (length(cmdArgs) == 0) {
  config.file.name <- def.config.file.name
}

LoadConfigFiles(report.config.file=config.file.name)
stock.tables <- Main(config.file.name)

