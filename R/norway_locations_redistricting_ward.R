
# Creates the norway_county_merging (fylkesammenslaaing) data.table
gen_norway_locations_redistricting_ward_internal <- function(x_year_end, x_year_start = 2005, include_extra_vars = F) {
  masterData <- data.table(readxl::read_excel(
    system.file("rawdata", "locations", "norway_locations_ward.xlsx", package = "fhidata"),
    col_types = c(
      "numeric",
      "numeric",
      "text",
      "numeric",
      "text",
      "text",
      "text",
      "text"
    )
  ))
  masterData[is.na(weighting), weighting := 1]

  masterData[year_start <= x_year_start, year_start := x_year_start]
  masterData <- masterData[year_start <= x_year_end]

  masterData <- masterData[year_start >= x_year_start | is.na(year_end)]
  setnames(masterData, "year_start", "year")

  masterData <- masterData[year_end >= x_year_start | is.na(year_end)]
  masterData <- masterData[year_end <= x_year_end | is.na(year_end)]
  masterData[year_end == x_year_end, ward_code_end := NA]
  masterData[year_end == x_year_end, year_end := NA]

  masterData[is.na(ward_code_end), ward_code_end := ward_code]
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
  setorder(skeleton, year, ward_code)

  merger <- unique(skeleton[ward_code != ward_code_end, c("ward_code", "ward_code_end", "weighting")])
  setnames(
    merger,
    c("ward_code_end", "weighting"),
    c("ward_code_end_new", "weighting_new")
  )

  continue_with_merging <- TRUE
  while (continue_with_merging) {
    print("merging!")
    skeleton <- merge(
      skeleton,
      merger,
      by.x = c("ward_code_end"),
      by.y = c("ward_code"),
      all.x = T
    )
    if (sum(!is.na(skeleton$ward_code_end_new)) == 0) {
      continue_with_merging <- FALSE
    }

    skeleton[!is.na(ward_code_end_new), ward_code_end := ward_code_end_new]
    skeleton[!is.na(weighting_new), weighting := weighting * weighting_new]
    skeleton[, ward_code_end_new := NULL]
    skeleton[, weighting_new := NULL]
  }

  skeletonFinal <- unique(skeleton[year == max(year), c(
    "ward_code",
    "ward_name",
    "municip_code",
    "municip_name"
  )])

  skeleton[, year_end := NULL]
  skeleton[, ward_name := NULL]
  skeleton[, municip_code := NULL]
  skeleton[, municip_name := NULL]

  skeleton <- merge(
    skeleton,
    skeletonFinal,
    by.x = c("ward_code_end"),
    by.y = c("ward_code")
  )

  setnames(skeleton, "ward_code_end", "ward_code_current")
  setnames(skeleton, "ward_code", "ward_code_original")

  setcolorder(
    skeleton,
    c(
      "ward_code_current",
      "ward_code_original",
      "year",
      "weighting",
      "ward_name",
      "municip_code",
      "municip_name"
    )
  )

  if (!include_extra_vars) {
    skeleton[, ward_name := NULL]
    skeleton[, municip_code := NULL]
    skeleton[, municip_name := NULL]
  }

  extra_years <- max(skeleton$year) + c(1:10)
  for (i in extra_years) {
    temp <- skeleton[year == max(year)]
    temp[, year := i]
    skeleton <- rbind(skeleton, temp)
  }

  skeleton <- skeleton[!stringr::str_detect(ward_code_current, "ward[0-9]")]

  return(invisible(skeleton))
}

gen_norway_locations_redistricting_ward <- function(x_year_end){
  stopifnot(x_year_end==2020)

  d <- gen_norway_locations_redistricting_ward_internal(
    x_year_end = x_year_end,
    x_year_start = 1910,
    include_extra_vars = FALSE
  )
  setnames(d, c("location_code_current", "location_code_original", "year", "weighting"))

  return(d)
}
