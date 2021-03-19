#' Redistricting in Norway (2020 borders).
#'
#' This dataset is used to transfer "original" datasets to the 2020 borders.
#'
#' Last updated 2021-02-15
#'
#' @format
#' \describe{
#' \item{location_code_current}{The location code per today.}
#' \item{location_code_original}{The location code as of 'year'.}
#' \item{calyear}{The year corresponding to 'county_code_original'.}
#' \item{weighting}{The weighting that needs to be applied.}
#' \item{granularity_geo}{nation/county/municip/wardbergen/wardoslo/wardstavanger/wardtrondheim/missingwardbergen/missingwardoslo/missingwardstavanger/missingwardtrondheim/notmainlandcounty/notmainlandmunicip/missingcounty}
#' }
"norway_locations_redistricting_b2020"

#' All redistricting in Norway (programable borders).
#'
#' @param include_year Do you want to include redistricting by year?
#' @param border The border year
#' @examples
#' norway_locations_redistricting()
#' @export
norway_locations_redistricting <- function(include_year = TRUE, border = fhidata::config$border){
  stopifnot(border==2020)
  if(border==2020){
    d <- copy(fhidata::norway_locations_redistricting_b2020)
  }
  if(!include_year){
    d <- unique(d[,.(location_code_current, location_code_original, granularity_geo)])
  }
  return(d)
}
