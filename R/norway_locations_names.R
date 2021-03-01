#' Names of areas in Norway that existed in 2020.
#'
#' @format
#' \describe{
#' \item{location_code}{Location code.}
#' \item{location_name}{Location name.}
#' \item{location_name_description_nb}{Location name with additional description.}
#' \item{location_name_file_nb_utf}{Location name that should be used in file names, with Norwegian characters.}
#' \item{location_name_file_nb_ascii}{Location name that should be used in file names, without Norwegian characters.}
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
    all[,.(location_code = missingward_code, location_name = missingward_name, group_order = 9)],

    all[,.(location_code = baregion_code, location_name = baregion_name, group_order = 10)],
    all[,.(location_code = region_code, location_name = region_name, group_order = 11)],
    all[,.(location_code = faregion_code, location_name = faregion_name, group_order = 12)]
  )
  d[, granularity_geo := get_granularity_geo(location_code)]
  d <- stats::na.omit(unique(d))

  setorder(d, group_order, location_code)
  d[, group_order := NULL]
  d[, location_order := 1:.N]

  # attach location name description
  d[granularity_geo== "nation", location_name_description_nb := location_name]

  d[granularity_geo== "county", location_name_description_nb := paste0(location_name, " (fylke)")]
  d[granularity_geo== "notmainlandcounty", location_name_description_nb := paste0(location_name, " (fylke)")]
  d[granularity_geo== "missingcounty", location_name_description_nb := paste0(location_name, " (fylke)")]

  d[norway_locations_hierarchy(from="municip",to="county",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (kommune i ", to_name, ")")
  ]
  d[norway_locations_hierarchy(from="notmainlandmunicip",to="notmainlandcounty",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (kommune i ", to_name, ")")
  ]
  d[norway_locations_hierarchy(from="missingmunicip",to="missingcounty",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (kommune i ", to_name, ")")
  ]

  d[norway_locations_hierarchy(from="wardoslo",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]
  d[norway_locations_hierarchy(from="wardbergen",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]
  d[norway_locations_hierarchy(from="wardtrondheim",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]
  d[norway_locations_hierarchy(from="wardstavanger",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]

  d[norway_locations_hierarchy(from="missingwardoslo",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]
  d[norway_locations_hierarchy(from="missingwardbergen",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]
  d[norway_locations_hierarchy(from="missingwardtrondheim",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]
  d[norway_locations_hierarchy(from="missingwardstavanger",to="municip",include_to_name = T, border = x_year_end),
    on="location_code==from_code",
    location_name_description_nb := paste0(location_name, " (bydel i ", to_name, ")")
  ]

  d[granularity_geo== "baregion", location_name_description_nb := paste0(location_name, " (BA-region)")]
  d[granularity_geo== "faregion", location_name_description_nb := paste0(location_name, " (Mattilsynet-region)")]
  d[granularity_geo== "region", location_name_description_nb := paste0(location_name, " (region)")]

  # location_name_file_nb_utf
  d[, location_name_file_nb_utf := location_name_description_nb]
  d[, location_name_file_nb_utf := stringr::str_replace_all(location_name_file_nb_utf, " ", "_")]
  d[, location_name_file_nb_utf := stringr::str_replace_all(location_name_file_nb_utf, "/", "_")]
  d[, location_name_file_nb_utf := stringr::str_remove_all(location_name_file_nb_utf, "\\.")]
  d[, location_name_file_nb_utf := stringr::str_remove_all(location_name_file_nb_utf, "\\(")]
  d[, location_name_file_nb_utf := stringr::str_remove_all(location_name_file_nb_utf, "\\)")]

  d[, location_name_file_nb_ascii := location_name_file_nb_utf]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, fhidata::nb$AA, "A")]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, fhidata::nb$aa, "a")]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, fhidata::nb$AE, "A")]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, fhidata::nb$ae, "a")]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, fhidata::nb$OE, "O")]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, fhidata::nb$oe, "o")]
  # stringi::stri_escape_unicode(stringi::stri_enc_toutf8())
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, "\\u00e1", "a")]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, "\\u0161", "s")]
  d[, location_name_file_nb_ascii := stringr::str_replace_all(location_name_file_nb_ascii, "\\u00fc", "u")]

  setcolorder(
    d,
    c(
      "location_code",
      "location_name",
      "location_name_description_nb",
      "location_name_file_nb_utf",
      "location_name_file_nb_ascii",
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


