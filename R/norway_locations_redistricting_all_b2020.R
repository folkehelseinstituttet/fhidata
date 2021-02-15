#' All redistricting in Norway (2020 borders).
#'
#' This dataset is used to transfer "original" datasets to the 2020 borders.
#'
#' Last updated 2021-02-15
#'
#' @format
#' \describe{
#' \item{location_code_current}{The location code per today.}
#' \item{location_code_original}{The location code as of 'year'.}
#' \item{year}{The year corresponding to 'county_code_original'.}
#' \item{weighting}{The weighting that needs to be applied.}
#' \item{granularity_geo}{nation/county/municip/wardbergen/wardoslo/wardstavanger/wardtrondheim/notmainlandcounty/notmainlandmunicip/missingcounty}
#' }
"norway_locations_redistricting_all_b2020"
