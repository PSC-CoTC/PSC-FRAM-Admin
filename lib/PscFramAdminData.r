################
#
# Common methods and constants for dealing with a FRAM Database
#
# Nicholas Komick
# nicholas.komick@dfo-mpo.gc.ca
# April 26, 2017
#
################

#' Retrieve a data frame with the relationships between CoTC members and FRAM fisheries
#'
#'
#' @return A data frame identifying who is resposible for each FRAM fishery catch reporting
#'
GetPersonFramFisheries <- function() {
  person.fishery <- ReadCsv("PersonFramFisheries.csv", data.dir, unique.col.names=c("fram.fishery.id"))
  return (person.fishery)
}