#' Hierarchy of different levels in Norway (2020 borders).
#'#'
#' Last updated 2021-02-15
#'
#' @format
#' \describe{
#' \item{municip_code}{The location code per today.}
#' \item{municip_name}{The location code as of 'year'.}
#' \item{baregion_code}{The location code per today.}
#' \item{baregion_name}{The location code as of 'year'.}
#' \item{county_code}{The location code as of 'year'.}
#' \item{county_name}{The location code as of 'year'.}
#' \item{region_code}{The location code as of 'year'.}
#' \item{region_name}{The location code as of 'year'.}
#' \item{faregion_name}{The location code as of 'year'.}
#' \item{faregion_code}{The location code as of 'year'.}
#' \item{ward_code}{The location code as of 'year'.}
#' \item{ward_name}{The location code as of 'year'.}
#' \item{missingward_code}{The location code as of 'year'.}
#' \item{missingward_name}{The location code as of 'year'.}
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

gen_norway_locations_hierarchy_missingward <- function(
  x_year_end,
  x_year_start = 1940
){

  d <- gen_norway_locations_redistricting_missingward(
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
  missing_ward <- gen_norway_locations_hierarchy_missingward(x_year_end = x_year_end, x_year_start = x_year_start)

  ward[, municip_name := NULL]
  ward[, ward_code_original := NULL]
  setnames(ward, "ward_code_current", "ward_code")

  missing_ward[, municip_name := NULL]
  missing_ward[, location_code_original := NULL]
  setnames(missing_ward, c("location_code_current", "location_name"), c("missingward_code","missingward_name"))

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

  d_1 <- merge(
    municip,
    missing_ward,
    by="municip_code",
    all=T
  )

  d_2 <- merge(
    municip,
    ward,
    by="municip_code",
    all=T
  )

  d <- rbind(
    d_1,
    d_2,
    fill=T
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

norway_locations_hierarchy_internal <- function(from, to, include_to_name = FALSE, border = fhidata::config$border){
  stopifnot(border==2020)
  stopifnot(from %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger",
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
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger",
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
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger"
  )){
    col_from <- "ward_code"
  } else if(from %in% c(
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger"
  )){
    col_from <- "missingward_code"
  } else {
    col_from <- paste0(from,"_code")
  }

  if(to %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger"
  )){
    col_to <- "ward_code"
    col_to_name <- "ward_name"
  } else if(to %in% c(
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger"
  )){
    col_to <- "missingward_code"
    col_to_name <- "missingward_name"
  } else {
    col_to <- paste0(to,"_code")
    col_to_name <- paste0(to,"_name")
  }

  if(include_to_name){
    d <- d[, c(col_from, col_to, col_to_name), with=F]
  } else {
    d <- d[, c(col_from, col_to), with=F]
  }
  d <- stats::na.omit(d)

  if(from %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger"
  )){
    d <- d[grep(paste0("^",from), get(col_from))]
    setnames(d, col_from, paste0(from,"_code"))
  }

  if(to %in% c(
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger"
  )){
    d <- d[grep(paste0("^",to), get(col_to))]
    setnames(d, col_to, paste0(to,"_code"))
  }
  d <- unique(d)

  if(ncol(d)==2){
    setnames(d, c("from_code","to_code"))
  } else {
    setnames(d, c("from_code","to_code", "to_name"))
  }

  return(d)
}



#' Hierarchies in Norway (programmable borders).
#'
#' @param from wardoslo, wardbergen, wardtrondheim, wardstavanger, municip, baregion, county, region, faregion, notmainlandmunicip, notmainlandcounty, missingmunicip, missingcounty
#' @param to wardoslo, wardbergen, wardtrondheim, wardstavanger, municip, baregion, county, region, faregion, notmainlandmunicip, notmainlandcounty, missingmunicip, missingcounty
#' @param include_to_name Do you want to include the name of the 'to' location?
#' @param border The border year
#' @examples
#' norway_locations_hierarchy(from="wardoslo", to="county")
#' norway_locations_hierarchy(from="municip", to="baregion")
#' @export
norway_locations_hierarchy <- function(from, to, include_to_name = FALSE, border = fhidata::config$border){
  plans <- expand.grid(
    from = from,
    to = to,
    stringsAsFactors = FALSE
  )
  retval <- vector("list", length=nrow(plans))
  for(i in seq_along(retval)){
    retval[[i]] <- norway_locations_hierarchy_internal(from=plans$from[i], to=plans$to[i], include_to_name, border)
  }
  retval <- unique(rbindlist(retval))
  retval
}
