#' Names of areas in Norway that existed in 2020.
#'
#' @format
#' \describe{
#' \item{municip_code}{Municipality code.}
#' \item{municip_name}{Municipality name.}
#' \item{county_code}{County code.}
#' \item{county_name}{County name.}
#' \item{region_code}{Region code.}
#' \item{region_name}{Region name.}
#' \item{faregion_code}{Food authority region code.}
#' \item{faregion_name}{Food authority region name.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"norway_locations_b2020"

#' Names of areas in Norway that existed in 2019.
#'
#' @format
#' \describe{
#' \item{municip_code}{Municipality code.}
#' \item{municip_name}{Municipality name.}
#' \item{county_code}{County code.}
#' \item{county_name}{County name.}
#' \item{region_code}{Region code.}
#' \item{region_name}{Region name.}
#' \item{faregion_code}{Food authority region code.}
#' \item{faregion_name}{Food authority region name.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"norway_locations_b2019"

#' Names of areas in Norway that existed in 2020.
#'
#' @format
#' \describe{
#' \item{ward_code}{Ward (bydel) code.}
#' \item{ward_name}{Ward (bydel) name.}
#' \item{municip_code}{Municipality code.}
#' \item{municip_name}{Municipality name.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"norway_locations_ward_b2020"

# Creates the norway_locations data.table
gen_norway_locations <- function(x_year_end) {

  # variables used by data.table
  is_current <- NULL
  year_end <- NULL
  #

  norway_locations <- gen_norway_municip_merging(x_year_end = x_year_end, include_extra_vars = T)

  unique(norway_locations[, c("municip_code_current", "municip_name",
                              "county_code", "county_name",
                              'faregion_name','faregion_code')])

  norway_locations <- norway_locations[year == max(year), c(
    "municip_code_current",
    "municip_name",
    "county_code",
    "county_name",
    "region_code",
    "region_name",
    'faregion_name',
    'faregion_code'
  )]


  norway_locations <- unique(norway_locations)
  setnames(norway_locations, "municip_code_current", "municip_code")

  return(norway_locations)
}

# Creates the norway_locations data.table
gen_norway_locations_ward <- function(x_year_end) {

  # variables used by data.table
  is_current <- NULL
  year_end <- NULL
  #

  norway_locations <- gen_norway_ward_merging(x_year_end = x_year_end, include_extra_vars = T)
  unique(norway_locations[, c("ward_code_current", "ward_name", "municip_code", "municip_name")])
  norway_locations <- norway_locations[year == max(year), c(
    "ward_code_current",
    "ward_name",
    "municip_code",
    "municip_name"
  )]
  norway_locations <- unique(norway_locations)
  setnames(norway_locations, "ward_code_current", "ward_code")

  return(norway_locations)
}
