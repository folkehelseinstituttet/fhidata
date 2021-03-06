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
#' \item{baregion_code}{Bo- og arbeids region code.}
#' \item{baregion_name}{Bo- og arbeids region name.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
#' @source \url{https://www.regjeringen.no/no/dokumenter/inndeling-av-kommuner-i-bo--og-arbeidsmarkedsregioner/id2662614/}
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

#' Names of areas in Norway (not mainland, i.e. Svalbard).
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
"norway_locations_notmainland_b2020"


#' Names of areas in Norway (missing).
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
"norway_locations_missing_b2020"


#' Names of municipality in Norway that existed in 2020.
#'
#'(same as norway_locations_b2020)
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
#' \item{baregion_code}{Bo- og arbeids region code.}
#' \item{baregion_name}{Bo- og arbeids region name.}
#' }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
"norway_locations_municip_b2020"


#' Names of wards (bydel, city district) in Norway that existed in 2020.
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
    'faregion_code',
    'faregion_name'
  )]


  norway_locations <- unique(norway_locations)
  setnames(norway_locations, "municip_code_current", "municip_code")

  # remove svalbard and missing
  norway_locations <- norway_locations[!municip_code %in% c("notmainlandmunicip2100",
                                                            "notmainlandmunicip2200",
                                                            "missingmunicip9999")]

  # if x_year_end = 2020 then include baregions (bo- og arbeidsregioner)
  if(x_year_end == 2020){
    ba <- data.table(readxl::read_excel(system.file("rawdata", "locations", "baregioner_2020.xlsx", package = "fhidata")))
    setnames(
      ba,
      1:2,
      c(
        "municip",
        "ba"
      )
    )
    ba[, municip_code := paste0(
      "municip",
      formatC(as.numeric(
        stringr::str_extract(municip, "^[0-9]+")),
        width=4,
        flag=0
      ))
    ]
    ba[, baregion_code := paste0(
      "baregion",
      formatC(as.numeric(
        stringr::str_extract(ba, "^[0-9]+")),
        width=3,
        flag=0
      ))
    ]
    ba[, baregion_name := stringr::str_remove_all(ba, "^[0-9]+ ")]
    norway_locations[
      ba,
      on="municip_code",
      baregion_code := baregion_code
    ]
    norway_locations[
      ba,
      on="municip_code",
      baregion_name := baregion_name
    ]


  }

  return(norway_locations)
}

# x_year_end <- 2020
gen_norway_locations_notmainland <- function(x_year_end){

  norway_locations <- gen_norway_notmainlandmunicip_merging(x_year_end = x_year_end, include_extra_vars = T)
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
    'faregion_code',
    'faregion_name'
  )]

  norway_locations <- unique(norway_locations)
  setnames(norway_locations, "municip_code_current", "municip_code")

  # take svalbard + jan mayen
  notmainland_locations <- norway_locations[municip_code %in% c("notmainlandmunicip2100",
                                                                "notmainlandmunicip2200")]

  notmainland_locations[, baregion_code := NA_character_]
  notmainland_locations[, baregion_name := NA_character_]

  return(notmainland_locations)
}




gen_norway_locations_missing <- function(x_year_end){

  norway_locations <- gen_norway_missingmunicip_merging(x_year_end = x_year_end, include_extra_vars = T)
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
    'faregion_code',
    'faregion_name'
  )]

  norway_locations <- unique(norway_locations)
  setnames(norway_locations, "municip_code_current", "municip_code")

  # take missing
  missing_locations <- norway_locations[municip_code == "missingmunicip9999"]

  missing_locations[, baregion_code := NA_character_]
  missing_locations[, baregion_name := NA_character_]

  return(missing_locations)
}



# this is the same as norway_locations
# we keep it here because we might need to delete the other one
gen_norway_locations_municip <- function(x_year_end) {

  # variables used by data.table
  is_current <- NULL
  year_end <- NULL

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
    'faregion_code',
    'faregion_name'
  )]


  norway_locations <- unique(norway_locations)
  setnames(norway_locations, "municip_code_current", "municip_code")

  # remove svalbard and missing
  norway_locations <- norway_locations[!municip_code %in% c("notmainlandmunicip2100",
                                                            "notmainlandmunicip2200",
                                                            "missingmunicip9999")]

  # if x_year_end = 2020 then include baregions (bo- og arbeidsregioner)
  if(x_year_end == 2020){
    ba <- data.table(readxl::read_excel(system.file("rawdata", "locations", "baregioner_2020.xlsx", package = "fhidata")))
    setnames(
      ba,
      1:2,
      c(
        "municip",
        "ba"
      )
    )
    ba[, municip_code := paste0(
      "municip",
      formatC(as.numeric(
        stringr::str_extract(municip, "^[0-9]+")),
        width=4,
        flag=0
      ))
    ]
    ba[, baregion_code := paste0(
      "baregion",
      formatC(as.numeric(
        stringr::str_extract(ba, "^[0-9]+")),
        width=3,
        flag=0
      ))
    ]
    ba[, baregion_name := stringr::str_remove_all(ba, "^[0-9]+ ")]
    norway_locations[
      ba,
      on="municip_code",
      baregion_code := baregion_code
    ]
    norway_locations[
      ba,
      on="municip_code",
      baregion_name := baregion_name
    ]


  }

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
