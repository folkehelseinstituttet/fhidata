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
"denmark_locations_names_b2020"

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
"sweden_locations_names_b2020"

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
"finland_locations_names_b2020"

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
"iceland_locations_names_b2020"

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
"denmark_population_by_age_b2020"

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
"sweden_population_by_age_b2020"

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
"finland_population_by_age_b2020"

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
"iceland_population_by_age_b2020"

# locations_long

gen_denmark_locations_names <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "locations", "locations_denmark_b2020.xlsx", package = "fhidata"))
  setDT(d)
  return(d)
}

gen_sweden_locations_names <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "locations", "locations_sweden_b2020.xlsx", package = "fhidata"))
  setDT(d)
  return(d)
}

gen_finland_locations_names <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "locations", "locations_finland_b2020.xlsx", package = "fhidata"))
  setDT(d)
  return(d)
}

gen_iceland_locations_names <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "locations", "locations_iceland_b2020.xlsx", package = "fhidata"))
  setDT(d)
  return(d)
}

# population

gen_denmark_population_by_age <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "population", "population_denmark_b2020.xlsx", package = "fhidata"))
  setDT(d)
  d1 <- copy(d)
  d1[, year := 2021]
  d <- rbind(d,d1)
  return(d)
}

gen_sweden_population_by_age <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "population", "population_sweden_b2020.xlsx", package = "fhidata"))
  setDT(d)
  d1 <- copy(d)
  d1[, year := 2021]
  d <- rbind(d,d1)
  return(d)
}

gen_finland_population_by_age <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "population", "population_finland_b2020.xlsx", package = "fhidata"))
  setDT(d)
  d1 <- copy(d)
  d1[, year := 2021]
  d <- rbind(d,d1)
  return(d)
}

gen_iceland_population_by_age <- function(x_year_end) {
  stopifnot(x_year_end == 2020)

  d <- readxl::read_excel(system.file("rawdata", "population", "population_iceland_b2020.xlsx", package = "fhidata"))
  setDT(d)
  d1 <- copy(d)
  d1[, year := 2021]
  d <- rbind(d,d1)
  return(d)
}










