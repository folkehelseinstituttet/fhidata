#' Names of areas in Denmark that existed in 2020.
#'
#' @format
#' \describe{
#' \item{iso3}{Location code.}
#' \item{granularity_geo}{nation, county, municip, ward}
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"denmark_locations_long_b2020"

#' Names of areas in Sweden that existed in 2020.
#'
#' @format
#' \describe{
#' \item{iso3}{Location code.}
#' \item{granularity_geo}{nation, county, municip, ward}
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"sweden_locations_long_b2020"

#' Names of areas in Finland that existed in 2020.
#'
#' @format
#' \describe{
#' \item{iso3}{Location code.}
#' \item{granularity_geo}{nation, county, municip, ward}
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"finland_locations_long_b2020"

#' Names of areas in Iceland that existed in 2020.
#'
#' @format
#' \describe{
#' \item{iso3}{Location code.}
#' \item{granularity_geo}{nation, county, municip, ward}
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"iceland_locations_long_b2020"

#' Names of areas in Denmark that existed in 2020.
#'
#' @format
#' \describe{
#' \item{year}{Year}
#' \item{granularity_geo}{nation, county, municip, ward}
#' \item{location_code}{Location code.}
#' \item{age}{Age.}
#' \item{pop}{Population.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"population_denmark_b2020"

#' Population of areas in Sweden that existed in 2020.
#'
#' @format
#' \describe{
#' \item{year}{Year}
#' \item{granularity_geo}{nation, county, municip, ward}
#' \item{location_code}{Location code.}
#' \item{age}{Age.}
#' \item{pop}{Population.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"population_sweden_b2020"

#' Names of areas in Finland that existed in 2020.
#'
#' @format
#' \describe{
#' \item{year}{Year}
#' \item{granularity_geo}{nation, county, municip, ward}
#' \item{location_code}{Location code.}
#' \item{age}{Age.}
#' \item{pop}{Population.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"population_finland_b2020"

#' Names of areas in Iceland that existed in 2020.
#'
#' @format
#' \describe{
#' \item{year}{Year}
#' \item{granularity_geo}{nation, county, municip, ward}
#' \item{location_code}{Location code.}
#' \item{age}{Age.}
#' \item{pop}{Population.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"population_iceland_b2020"

# locations_long

gen_data_denmark_locations_long <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("extdata", "locations_denmark_b2020.xlsx", package = "fhidata"))
  setDT(d)
  return(d)
}

gen_data_sweden_locations_long <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("extdata", "locations_sweden_b2020.xlsx", package = "fhidata"))
  setDT(d)
  return(d)
}

gen_data_finland_locations_long <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("extdata", "locations_finland_b2020.xlsx", package = "fhidata"))
  setDT(d)
  return(d)
}

gen_data_iceland_locations_long <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("extdata", "locations_iceland_b2020.xlsx", package = "fhidata"))
  setDT(d)
  return(d)
}

# population

gen_data_denmark_population <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("extdata", "population_denmark_b2020.xlsx", package = "fhidata"))
  setDT(d)
  return(d)
}

gen_data_sweden_population <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("extdata", "population_sweden_b2020.xlsx", package = "fhidata"))
  setDT(d)
  return(d)
}

gen_data_finland_population <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("extdata", "population_finland_b2020.xlsx", package = "fhidata"))
  setDT(d)
  return(d)
}

gen_data_iceland_population <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("extdata", "population_iceland_b2020.xlsx", package = "fhidata"))
  setDT(d)
  return(d)
}
