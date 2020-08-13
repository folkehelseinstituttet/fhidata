#' Names of areas in Norway that existed in 2020.
#'
#' @format
#' \describe{
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' \item{location_order}{The preferred presentation order}
#' \item{granularity_geo}{nation, county, municip, ward}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"norway_locations_long_b2020"

#' Names of areas in Norway that existed in 2019.
#'
#' @format
#' \describe{
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' \item{location_order}{The preferred presentation order}
#' \item{granularity_geo}{nation, county, municip, ward}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"norway_locations_long_b2019"

# Creates the norway_locations data.table
gen_norway_locations_long <- function(x_year_end) {
  a <- data.table(location_code = "norge", location_name = "Norge")
  a[, granularity_geo := "nation"]
  b <- gen_norway_locations(x_year_end = x_year_end)[, c("county_code", "county_name")]
  b[, granularity_geo := "county"]
  c <- gen_norway_locations(x_year_end = x_year_end)[, c("municip_code", "municip_name")]
  c[, granularity_geo := "municip"]
  d <- gen_norway_locations_ward(x_year_end = x_year_end)[, c("ward_code", "ward_name")]
  d[, granularity_geo := "ward"]
  setnames(b, c("location_code", "location_name", "granularity_geo"))
  setorder(b, location_code)
  setnames(c, c("location_code", "location_name", "granularity_geo"))
  setorder(c, location_code)
  setnames(d, c("location_code", "location_name", "granularity_geo"))
  setorder(d, location_code)

  retval <- unique(rbind(a, b, c, d))
  retval[, location_order := 1:.N]
  setcolorder(retval, c("location_code", "location_name", "location_order", "granularity_geo"))

  return(retval)
}
