
# Creates the norway_municip_merging (kommunesammenslaaing) data.table
gen_norway_locations_redistricting_missingmunicip_internal <- function(
  x_year_end,
  x_year_start = 2000,
  include_extra_vars = FALSE) {
  # variables used in data.table functions in this function
  year_start <- NULL
  municip_code <- NULL
  municip_code_current <- NULL
  level <- NULL
  county_code <- NULL
  region_code <- NULL
  year_end <- NULL
  municip_name <- NULL
  municip_code_end <- NULL
  county_name <- NULL
  region_name <- NULL
  faregion_name <- NULL
  faregion_code <- NULL
  realEnd <- NULL
  weighting <- NULL
  municip_code_end_new <- NULL
  weighting_new <- NULL
  # end

  masterData <- data.table(readxl::read_excel(system.file("rawdata", "locations", "norway_locations.xlsx", package = "fhidata")))
  masterData <- masterData[county_code == "missingcounty99"]
  masterData[is.na(weighting), weighting := 1]

  masterData[year_start <= x_year_start, year_start := x_year_start]
  masterData <- masterData[year_start <= x_year_end]

  masterData <- masterData[year_start >= x_year_start | is.na(year_end)]
  setnames(masterData, "year_start", "year")

  masterData <- masterData[year_end >= x_year_start | is.na(year_end)]
  masterData <- masterData[year_end <= x_year_end | is.na(year_end)]
  masterData[year_end == x_year_end, municip_code_end := NA]
  masterData[year_end == x_year_end, year_end := NA]

  masterData[is.na(municip_code_end), municip_code_end := municip_code]
  masterData[is.na(year_end), year_end := x_year_end]

  retval <- vector("list", 10000)
  for (i in 1:nrow(masterData)) {
    p <- masterData[i, ]
    years <- p$year:p$year_end
    temp <- p[rep(1, length(years))]
    temp[, year := years]
    retval[[i]] <- temp
  }
  skeleton <- rbindlist(retval)
  setorder(skeleton, year, municip_code)

  # skeleton <- skeleton[municip_code %in% c("municip1613","municip5012","municip5059")]

  merger <- unique(skeleton[municip_code != municip_code_end, c("municip_code", "municip_code_end", "weighting")])
  setnames(
    merger,
    c("municip_code_end", "weighting"),
    c("municip_code_end_new", "weighting_new")
  )

  continue_with_merging <- TRUE
  while (continue_with_merging) {
    print("merging!")
    skeleton <- merge(
      skeleton,
      merger,
      by.x = c("municip_code_end"),
      by.y = c("municip_code"),
      all.x = T
    )
    if (sum(!is.na(skeleton$municip_code_end_new)) == 0) {
      continue_with_merging <- FALSE
    }

    skeleton[!is.na(municip_code_end_new), municip_code_end := municip_code_end_new]
    skeleton[!is.na(weighting_new), weighting := weighting * weighting_new]
    skeleton[, municip_code_end_new := NULL]
    skeleton[, weighting_new := NULL]
  }

  skeletonFinal <- unique(skeleton[year == max(year), c(
    "municip_code",
    "municip_name",
    "county_code",
    "county_name",
    "region_code",
    "region_name",
    'faregion_name',
    'faregion_code'
  )])

  skeleton[, year_end := NULL]
  skeleton[, municip_name := NULL]
  skeleton[, county_code := NULL]
  skeleton[, county_name := NULL]
  skeleton[, region_code := NULL]
  skeleton[, region_name := NULL]
  skeleton[, faregion_code := NULL]
  skeleton[, faregion_name := NULL]


  skeleton <- merge(
    skeleton,
    skeletonFinal,
    by.x = c("municip_code_end"),
    by.y = c("municip_code")
  )

  setnames(skeleton, "municip_code_end", "municip_code_current")
  setnames(skeleton, "municip_code", "municip_code_original")

  setcolorder(
    skeleton,
    c(
      "municip_code_current",
      "municip_code_original",
      "year",
      "weighting",
      "municip_name",
      "county_code",
      "county_name",
      "region_code",
      "region_name",
      'faregion_name',
      'faregion_code'

    )
  )

  if (!include_extra_vars) {
    skeleton[, municip_name := NULL]
    skeleton[, county_code := NULL]
    skeleton[, county_name := NULL]
    skeleton[, region_code := NULL]
    skeleton[, region_name := NULL]
    skeleton[, faregion_code := NULL]
    skeleton[, faregion_name := NULL]
  }

  extra_years <- max(skeleton$year) + c(1:10)
  for (i in extra_years) {
    temp <- skeleton[year == max(year)]
    temp[, year := i]
    skeleton <- rbind(skeleton, temp)
  }

  return(invisible(skeleton))
}


gen_norway_missingcounty_merging <- function(
  x_year_end,
  x_year_start = 2000){

  retval <- data.table(county_code_current = "missingcounty99",
                       county_code_orginal = "missingcounty99",
                       year = seq(x_year_start, x_year_end, by = 1),
                       weighting = 1)


return(invisible(retval))

}



gen_norway_locations_redistricting_missingmunicip <- function(
  x_year_end
){
  d <- gen_norway_locations_redistricting_notmainlandmunicip_internal(
    x_year_end = x_year_end,
    x_year_start = 1940
  )
  setnames(d, c("location_code_current", "location_code_original", "year", "weighting"))

  return(d)
}


gen_norway_locations_redistricting_missingcounty <- function(
  x_year_end,
  x_year_start = 1940){

  retval <- data.table(location_code_current = "missingcounty99",
                       location_code_original = "missingcounty99",
                       year = seq(x_year_start, x_year_end, by = 1),
                       weighting = 1)

  return(invisible(retval))

}

gen_norway_locations_redistricting_missingmunicip <- function(
  x_year_end
){
  d <- gen_norway_locations_redistricting_notmainlandmunicip_internal(
    x_year_end = x_year_end,
    x_year_start = 1940
  )
  setnames(d, c("location_code_current", "location_code_original", "year", "weighting"))

  return(d)
}

gen_norway_locations_redistricting_missingward <- function(
  x_year_end,
  x_year_start = 1940,
  include_extra_vars = FALSE){

  retval <- list()
  retval[[length(retval)+1]] <- data.table(
    location_code_current = "missingwardoslo030199",
    location_code_original = "missingwardoslo030199",
    location_name = "Ukjent bydel i Oslo",
    year = seq(x_year_start, x_year_end, by = 1),
    weighting = 1,
    municip_code = "municip0301",
    municip_name = "Oslo"
  )

  retval[[length(retval)+1]] <- data.table(
    location_code_current = "missingwardbergen460199",
    location_code_original = "missingwardbergen460199",
    location_name = "Ukjent bydel i Bergen",
    year = seq(x_year_start, x_year_end, by = 1),
    weighting = 1,
    municip_code = "municip4601",
    municip_name = "Bergen"
  )

  retval[[length(retval)+1]] <- data.table(
    location_code_current = "missingwardtrondheim500199",
    location_code_original = "missingwardtrondheim500199",
    location_name = "Ukjent bydel i Trondheim",
    year = seq(x_year_start, x_year_end, by = 1),
    weighting = 1,
    municip_code = "municip5001",
    municip_name = "Trondheim"
  )

  retval[[length(retval)+1]] <- data.table(
    location_code_current = "missingwardstavanger110399",
    location_code_original = "missingwardstavanger110399",
    location_name = "Ukjent bydel i Stavanger",
    year = seq(x_year_start, x_year_end, by = 1),
    weighting = 1,
    municip_code = "municip1103",
    municip_name = "Stavanger"
  )

  retval <- rbindlist(retval)

  if (!include_extra_vars) {
    retval[, location_name := NULL]
    retval[, municip_code := NULL]
    retval[, municip_name := NULL]
  }

  return(invisible(retval))

}






