
# Creates the norway_county_merging (fylkesammenslaaing) data.table
gen_norway_locations_redistricting_county_internal <- function(x_year_end, x_year_start = 2000) {
  # variables used in data.table functions in this function
  . <- NULL
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
  realEnd <- NULL
  weighting <- NULL
  imputed <- NULL
  pop <- NULL
  location_code <- NULL
  county_code_original <- NULL
  municip_code_original <- NULL
  county_code_current <- NULL
  weighting_denominator_from_original <- NULL
  border_end <- NULL
  border_start <- NULL
  municip_code_end_new <- NULL
  weighting_new <- NULL
  # end

  municips <- gen_norway_locations_redistricting_municip_internal(
    x_year_end = x_year_end,
    x_year_start = x_year_start
  )

  pops0 <- gen_norway_population(x_year_end = x_year_end, original = TRUE)
  pops0 <- pops0[imputed == FALSE, .(pop = sum(pop)), keyby = .(municip_code, year)]

  pops1 <- gen_norway_population(x_year_end = x_year_end)
  pops1 <- pops1[imputed == TRUE & level == "municipality", .(pop = sum(pop)), keyby = .(municip_code = location_code, year)]

  pops <- rbind(pops0, pops1)

  x <- merge(
    municips,
    pops,
    by.x = c("municip_code_original", "year"),
    by.y = c("municip_code", "year"),
  )
  x[, county_code_original := stringr::str_sub(municip_code_original, 1, 9)]
  x[, county_code_current := stringr::str_sub(municip_code_current, 1, 9)]

  x[, county_code_original := stringr::str_replace(county_code_original, "municip", "county")]
  x[, county_code_current := stringr::str_replace(county_code_current, "municip", "county")]

  x[, weighting := weighting * pop]
  x <- x[, .(
    weighting = sum(weighting)
  ), keyby = .(
    year,
    county_code_original,
    county_code_current
  )]
  x[, weighting_denominator_from_original := sum(weighting), by = .(county_code_original, year)]
  x[, weighting := weighting / weighting_denominator_from_original]
  x[, weighting_denominator_from_original := NULL]

  for (i in 1:30) {
    temp <- x[year == min(year)]
    temp[, year := year - 1]
    x <- rbind(temp, x)
  }

  extra_years <- max(x$year) + c(1:10)
  for (i in extra_years) {
    temp <- x[year == max(year)]
    temp[, year := i]
    x <- rbind(x, temp)
  }

  return(invisible(x))
}

gen_norway_locations_redistricting_county <- function(x_year_end){
  stopifnot(x_year_end==2020)

  d <- gen_norway_locations_redistricting_county_internal(
    x_year_end = x_year_end,
    x_year_start = 1910
  )
  setcolorder(
    d,
    c(
      "county_code_current",
      "county_code_original",
      "year",
      "weighting"
    )
  )
  setnames(d, c("location_code_current", "location_code_original", "year", "weighting"))

  return(d)
}


