#' Hierarchy of different levels in Norway (2020 borders).
#'#'
#' Last updated 2021-02-15
#'
#' @format
#' \describe{
#' \item{municip_code}{The location code per today.}
#' \item{municip_name}{The location code as of 'year'.}
#' \item{county_code}{The location code as of 'year'.}
#' \item{county_name}{The location code as of 'year'.}
#' \item{region_code}{The location code as of 'year'.}
#' \item{region_name}{The location code as of 'year'.}
#' \item{faregion_name}{The location code as of 'year'.}
#' \item{faregion_code}{The location code as of 'year'.}
#' \item{ward_code}{The location code as of 'year'.}
#' \item{ward_name}{The location code as of 'year'.}
#' \item{notmainlandmunicip_code}{The location code as of 'year'.}
#' \item{notmainlandmunicip_name}{The location code as of 'year'.}
#' \item{notmainlandcounty_code}{The location code as of 'year'.}
#' \item{notmainlandcounty_name}{The location code as of 'year'.}
#' \item{missingmunicip_code}{The location code as of 'year'.}
#' \item{missingmunicip_name}{The location code as of 'year'.}
#' \item{missingcounty_code}{The location code as of 'year'.}
#' \item{missingcounty_name}{The location code as of 'year'.}
#' }
"norway_locations_hierarchy_all_b2020"

gen_norway_locations_hierarchy_municip <- function(
  x_year_end,
  x_year_start = 1940
  ){

  d <- gen_norway_locations_redistricting_municip_internal(
    x_year_end = x_year_end,
    x_year_start = 1940,
    include_extra_vars = T
  )[year==max(year)]
  d[, year := NULL]
  d[, weighting := NULL]

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
  d[
    ba,
    on="municip_code_current==municip_code",
    baregion_code := baregion_code
  ]
  d[
    ba,
    on="municip_code_current==municip_code",
    baregion_name := baregion_name
  ]

  return(invisible(d))
}

gen_norway_locations_hierarchy_ward <- function(
  x_year_end,
  x_year_start = 1940
  ){

  d <- gen_norway_locations_redistricting_ward_internal(
    x_year_end = x_year_end,
    x_year_start = 1940,
    include_extra_vars = T
  )[year==max(year)]
  d[, year := NULL]
  d[, weighting := NULL]

  return(invisible(d))
}

gen_norway_locations_hierarchy_notmainland <- function(
  x_year_end,
  x_year_start = 1940
  ){

  d <- gen_norway_locations_redistricting_notmainlandmunicip_internal(
    x_year_end = x_year_end,
    x_year_start = 1940,
    include_extra_vars = T
  )[year==max(year)]
  d[, year := NULL]
  d[, weighting := NULL]

  return(invisible(d))
}

gen_norway_locations_hierarchy_missing <- function(
  x_year_end,
  x_year_start = 1940
){

  d <- gen_norway_locations_redistricting_missingmunicip_internal(
    x_year_end = x_year_end,
    x_year_start = 1940,
    include_extra_vars = T
  )[year==max(year)]
  d[, year := NULL]
  d[, weighting := NULL]

  return(invisible(d))
}

gen_norway_locations_hierarchy_all <- function(
  x_year_end = 2020,
  x_year_start = 1940
){

  ward <- gen_norway_locations_hierarchy_ward(x_year_end = x_year_end, x_year_start = x_year_start)
  municip <- gen_norway_locations_hierarchy_municip(x_year_end = x_year_end, x_year_start = x_year_start)
  notmainland <- gen_norway_locations_hierarchy_notmainland(x_year_end = x_year_end, x_year_start = x_year_start)
  missing <- gen_norway_locations_hierarchy_missing(x_year_end = x_year_end, x_year_start = x_year_start)

  ward[, municip_name := NULL]
  ward[, ward_code_original := NULL]
  setnames(ward, "ward_code_current", "ward_code")

  municip[, municip_code_original := NULL]
  setnames(municip, "municip_code_current", "municip_code")

  notmainland[, municip_code_original := NULL]
  setnames(
    notmainland,
    c("municip_code_current", "municip_name", "county_code", "county_name"),
    c("notmainlandmunicip_code", "notmainlandmunicip_name", "notmainlandcounty_code", "notmainlandcounty_name"),
  )

  missing[, municip_code_original := NULL]
  setnames(
    missing,
    c("municip_code_current", "municip_name", "county_code", "county_name"),
    c("missingmunicip_code", "missingmunicip_name", "missingcounty_code", "missingcounty_name"),
  )


  d <- merge(
    municip,
    ward,
    by="municip_code",
    all=T
  )
  d <- rbind(
    d,
    notmainland,
    fill = TRUE
  )
  d <- rbind(
    d,
    missing,
    fill = TRUE
  )

  return(d)
}

#' Hierarchies in Norway (programable borders).
#'
#' @param from wardoslo, wardbergen, wardtrondheim, wardstavanger, municip, baregion, county, region, faregion, notmainlandmunicip, notmainlandcounty, missingmunicip, missingcounty
#' @param border The border year
#' @examples
#' norway_locations_hierarchy(from="wardoslo", to="county")
#' norway_locations_hierarchy(from="municip", to="baregion")
#' @export
norway_locations_hierarchy <- function(from, to, border = fhidata::config$border){
  stopifnot(border==2020)
  stopifnot(from %in% c(
    "wardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "municip",
    "baregion",
    "county",
    "region",
    "faregion",
    "notmainlandmunicip",
    "notmainlandcounty",
    "missingmunicip",
    "missingcounty"
  ))
  stopifnot(to %in% c(
    "wardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "municip",
    "baregion",
    "county",
    "region",
    "faregion",
    "notmainlandmunicip",
    "notmainlandcounty",
    "missingmunicip",
    "missingcounty"
  ))
  if(border==2020){
    d <- fhidata::norway_locations_hierarchy_all_b2020
  }

  if(from %in% c(
    "wardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger"
  )){
    col_from <- "ward_code"
  } else {
    col_from <- paste0(from,"_code")
  }

  if(to %in% c(
    "wardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger"
  )){
    col_to <- "ward_code"
  } else {
    col_to <- paste0(to,"_code")
  }

  d <- d[, c(col_from, col_to), with=F]
  d <- stats::na.omit(d)

  if(from %in% c(
    "wardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger"
  )){
    d <- d[grep(from, get(col_from))]
  }

  if(to %in% c(
    "wardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger"
  )){
    d <- d[grep(to, get(col_to))]
  }

  return(d)
}


