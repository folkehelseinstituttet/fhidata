#' Names of areas in Norway that existed in 2020.
#'
#' @format
#' \describe{
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"norway_locations_long_b2020"

#' Names of areas in Norway that existed in 2019.
#'
#' @format
#' \describe{
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"norway_locations_long_b2019"

# Creates the norway_locations data.table
gen_norway_locations_long <- function(x_year_end) {
  a1 <- data.table(location_code = "norway", location_name = "Norway")
  a2 <- data.table(location_code = "norge", location_name = "Norge")
  b <- gen_norway_locations(x_year_end = x_year_end)[, c("municip_code", "municip_name")]
  c <- gen_norway_locations(x_year_end = x_year_end)[, c("county_code", "county_name")]
  d <- gen_norway_locations_ward(x_year_end = x_year_end)[, c("ward_code", "ward_name")]
  setnames(b, c("location_code", "location_name"))
  setnames(c, c("location_code", "location_name"))
  setnames(d, c("location_code", "location_name"))

  retval <- unique(rbind(a1, a2, b, c, d))

  return(retval)
}
