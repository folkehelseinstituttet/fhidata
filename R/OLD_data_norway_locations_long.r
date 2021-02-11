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
  b1 <- gen_norway_locations_notmainland(x_year_end = x_year_end)[, c("county_code", "county_name")]
  b1[county_code == "notmainlandcounty21", granularity_geo := "notmainlandcounty"]
  b1[county_code == "notmainlandcounty22", granularity_geo := "notmainlandcounty"]
  b2 <- gen_norway_locations_missing(x_year_end = x_year_end)[, c("county_code", "county_name")]
  b2[county_code == "missingcounty99", granularity_geo := "missingcounty"]
  b <- rbind(b, b1, b2)

  # municip
  c <- gen_norway_locations(x_year_end = x_year_end)[, c("municip_code", "municip_name")]
  c[, granularity_geo := "municip"]
  c1 <- gen_norway_locations_notmainland(x_year_end = x_year_end)[, c("municip_code", "municip_name")]
  c1[municip_code %in% c("notmainlandmunicip2111",
                        "notmainlandmunicip2121",
                        "notmainlandmunicip2131",
                        "notmainlandmunicip2100",
                        "notmainlandmunicip2200"),
    granularity_geo := "notmainlandmunicip"]
  c2 <- gen_norway_locations_missing(x_year_end = x_year_end)[, c("municip_code", "municip_name")]
  c2[municip_code == 'missingmunicip9999', granularity_geo := 'missingmunicip']
  c <- rbind(c, c1, c2)

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

  setnames(f, c("location_code", "location_name", "granularity_geo"))
  setorder(f, location_code)
  retval <- rbind(retval, f, fill = T)

  # attach location name description
  retval[granularity_geo== "nation", location_name_description := location_name]
  retval[granularity_geo== "county", location_name_description := paste0(location_name, " (fylke)")]
  retval[all, on = "location_code==municip_code", location_name_description := paste0(location_name, " (kommune i ", county_name,")")]
  retval[all_ward, on = "location_code==ward_code", location_name_description := paste0(location_name, " (bydel i ", municip_name,")")]
  retval[granularity_geo== "baregion", location_name_description := paste0(location_name, " (BA-region)")]

  # location name description for missing and svalbard, jan mayen
  retval[location_code == "notmainlandcounty21", location_name_description := "Utenfor fastlands-Norge (Svalbard)"]
  retval[location_code == "notmainlandcounty22", location_name_description := "Utenfor fastlands-Norge (Jan Mayen)"]
  retval[location_code == "notmainlandmunicip2100", location_name_description := "Utenfor fastlands-Norge (Svalbard)"]
  retval[location_code == "notmainlandmunicip2200", location_name_description := "Utenfor fastlands-Norge (Jan Mayen)"]
  retval[location_code == "missingcounty99", location_name_description := "Ukjent fykle"]
  retval[location_code == "missingmunicip9999", location_name_description := "Ukjent kommune"]

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


