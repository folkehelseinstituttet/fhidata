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
"norway_locations_names_b2020"

# Creates the norway_locations data.table
gen_norway_locations_names <- function(x_year_end = 2020) {
  all <- gen_norway_locations_hierarchy_all(x_year_end = x_year_end)

  d <- rbind(
    data.table(location_code = "norge", location_name = "Norge", group_order = 1),

    all[,.(location_code = county_code, location_name = county_name, group_order = 2)],
    all[,.(location_code = notmainlandcounty_code, location_name = notmainlandcounty_name, group_order = 3)],
    all[,.(location_code = missingcounty_code, location_name = missingcounty_name, group_order = 4)],

    all[,.(location_code = municip_code, location_name = municip_name, group_order = 5)],
    all[,.(location_code = notmainlandmunicip_code, location_name = notmainlandmunicip_name, group_order = 6)],
    all[,.(location_code = missingmunicip_code, location_name = missingmunicip_name, group_order = 7)],

    all[,.(location_code = ward_code, location_name = ward_name, group_order = 8)],

    all[,.(location_code = baregion_code, location_name = baregion_name, group_order = 9)],
    all[,.(location_code = region_code, location_name = region_name, group_order = 10)],
    all[,.(location_code = faregion_code, location_name = faregion_name, group_order = 11)]
  )
  d[, granularity_geo := get_granularity_geo(location_code)]
  d <- na.omit(unique(d))

  setorder(d, group_order, location_code)
  d[, group_order := NULL]
  d[, location_order := 1:.N]

  # attach location name description
  d[granularity_geo== "nation", location_name_description := location_name]

  d[granularity_geo== "county", location_name_description := paste0(location_name, " (fylke)")]
  d[granularity_geo== "notmainlandcounty", location_name_description := paste0(location_name, " (fylke)")]
  d[granularity_geo== "missingcounty", location_name_description := paste0(location_name, " (fylke)")]

  d[norway_locations_hierarchy(from="municip",to="county",include_to_name = T, border = x_year_end),
    on="location_code==municip_code",
    location_name_description := paste0(location_name, "(kommune i ", county_name)
  ]
  d[norway_locations_hierarchy(from="notmainlandmunicip",to="notmainlandcounty",include_to_name = T, border = x_year_end),
    on="location_code==notmainlandmunicip_code",
    location_name_description := paste0(location_name, "(kommune i ", notmainlandcounty_name)
  ]
  d[norway_locations_hierarchy(from="missingmunicip",to="missingcounty",include_to_name = T, border = x_year_end),
    on="location_code==missingmunicip_code",
    location_name_description := paste0(location_name, "(kommune i ", missingcounty_name)
  ]

  d[norway_locations_hierarchy(from="wardoslo",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==wardoslo_code",
    location_name_description := paste0(location_name, "(bydel i ", municip_name)
  ]
  d[norway_locations_hierarchy(from="wardbergen",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==wardbergen_code",
    location_name_description := paste0(location_name, "(bydel i ", municip_name)
  ]
  d[norway_locations_hierarchy(from="wardtrondheim",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==wardtrondheim_code",
    location_name_description := paste0(location_name, "(bydel i ", municip_name)
  ]
  d[norway_locations_hierarchy(from="wardstavanger",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==wardstavanger_code",
    location_name_description := paste0(location_name, "(bydel i ", municip_name)
  ]

  d[granularity_geo== "baregion", location_name_description := paste0(location_name, " (BA-region)")]
  d[granularity_geo== "faregion", location_name_description := paste0(location_name, " (Mattilsynet-region)")]
  d[granularity_geo== "region", location_name_description := paste0(location_name, " (region)")]

  setcolorder(
    d,
    c(
      "location_code",
      "location_name",
      "location_name_description",
      "location_order",
      "granularity_geo"
    )
  )

  return(d)
}

#' All names in Norway (programmable borders).
#'
#' @param border The border year
#' @examples
#' norway_locations_names()
#' @export
norway_locations_names <- function(border = fhidata::config$border){
  stopifnot(border==2020)
  if(border==2020){
    d <- copy(fhidata::norway_locations_names_b2020)
  }
  return(d)
}


