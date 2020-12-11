#' Names of areas in Norway that existed in 2020.
#'
#' @format
#' \describe{
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' \item{location_name_description}{Location name with additional description.}
#' \item{location_order}{The preferred presentation order}
#' \item{granularity_geo}{nation, county, municip, wardoslo, wardbergen, wardstavanger, wardtrondheim, baregion}
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
  all <- gen_norway_locations(x_year_end = x_year_end)
  all_ward <- gen_norway_locations_ward(x_year_end = x_year_end)

  # nation
  a <- data.table(location_code = "norge", location_name = "Norge")
  a[, granularity_geo := "nation"]

  # county
  b <- gen_norway_locations(x_year_end = x_year_end)[, c("county_code", "county_name")]
  b[, granularity_geo := "county"]
  # b[county_code == 'county21', granularity_geo := 'countynotmainland']
  # b[county_code == 'county99', granularity_geo := 'countymissing']


  # municip
  c <- gen_norway_locations(x_year_end = x_year_end)[, c("municip_code", "municip_name")]
  c[, granularity_geo := "municip"]
  c[municip_code %in% c('municip2111',
                         'municip2121',
                         'municip2131',
                         'municip2100'),
    granularity_geo := 'municipnotmainland']
  # c[municip_code == 'municip9999', granularity_geo := 'municipmissing']

  # ward
  d <- gen_norway_locations_ward(x_year_end = x_year_end)[, c("ward_code", "ward_name")]
  d[, granularity_geo := stringr::str_extract(ward_code,"^[a-z]+")]
  setnames(b, c("location_code", "location_name", "granularity_geo"))
  setorder(b, location_code)
  setnames(c, c("location_code", "location_name", "granularity_geo"))
  setorder(c, location_code)
  setnames(d, c("location_code", "location_name", "granularity_geo"))
  setorder(d, location_code)

  retval <- unique(rbind(a, b, c, d, fill = T))

  # baregion
  f <- unique(na.omit(gen_norway_locations(x_year_end = x_year_end)[, c("baregion_code", "baregion_name")]))
  f[, granularity_geo := "baregion"]
  # f[baregion_code == 'baregion0', granularity_geo := 'baregionnotmainland']
  # f[baregion_code == 'baregion9', granularity_geo := 'baregionmissing']

  setnames(f, c("location_code", "location_name", "granularity_geo"))
  setorder(f, location_code)
  retval <- rbind(retval, f, fill = T)

  # attach location name description
  retval[granularity_geo== "nation", location_name_description := location_name]
  retval[granularity_geo== "county", location_name_description := paste0(location_name, " (fylke)")]
  retval[all, on = "location_code==municip_code", location_name_description := paste0(location_name, " (kommune i ", county_name,")")]
  retval[all_ward, on = "location_code==ward_code", location_name_description := paste0(location_name, " (bydel i ", municip_name,")")]
  retval[granularity_geo== "baregion", location_name_description := paste0(location_name, " (BA-region)")]

  # location name description for missing and svalbard
  # retval[location_code == 'county21' & granularity_geo == 'countynotmainland', location_name_description:= 'Utenfor fastlands-Norge (fylke)']
  # retval[location_code == 'county99' & granularity_geo == 'countymissing', location_name_description:= 'Ukjent region (fylke)']
  # retval[location_code == 'baregion0' & granularity_geo == 'baregionnotmainland', location_name_description:= 'Utenfor fastlands-Norge (BA-region)']
  # retval[location_code == 'baregion9' & granularity_geo == 'baregionmissing', location_name_description:= 'Ukjent region (BA-region)']


  retval[, location_order := 1:.N]
  setcolorder(
    retval,
    c(
      "location_code",
      "location_name",
      "location_name_description",
      "location_order",
      "granularity_geo"
    )
  )

  return(retval)
}
