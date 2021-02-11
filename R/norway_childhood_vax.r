#' Childhood vaccination rates in Norway (2020 borders)
#'
#' We conveniently package vaccine coverage data taken from "Kommunehelsa statistikkbank".
#' This data was last updated on 2019-04-09.
#'
#' This dataset contains national/county/municipality level (5 year average) vaccination coverage rates
#' for 16 year olds for the childhood vaccination program (diphtheria, hpv, measles,
#' mumps, poliomyelitis, pertussis, rubella, tetanus).
#'
#' Municipalities are updated for the 2020 borders.
#'
#' @format
#' \describe{
#' \item{year}{The middle year of a 5 year range (e.g. 2011 is the average of data from 2009-2013).}
#' \item{location_code}{The location code.}
#' \item{age}{The population age.}
#' \item{vax}{The vaccine.}
#' \item{proportion}{Proportion of people who are vaccinated.}
#' \item{imputed}{FALSE if real data. TRUE if it is the national average.}
#' }
#' @source \url{http://khs.fhi.no/webview/}
"norway_childhood_vax_b2020"

# Creates the childhood vaccination dataset
# http://khs.fhi.no/webview/
#' @import data.table
gen_norway_childhood_vax <- function(x_year_end) {
  stopifnot(x_year_end %in% c(2019, 2020))

  # variables used in data.table functions in this function
  . <- NULL
  GEO <- NULL
  location_code <- NULL
  AAR <- NULL
  SPVFLAGG <- NULL
  ALDER <- NULL
  proportion <- NULL
  RATE <- NULL
  vax <- NULL
  VAKSINE <- NULL
  national <- NULL
  age <- NULL
  imputed <- NULL
  year_merging <- NULL
  weighting <- NULL
  county_code_current <- NULL
  KJONN <- NULL
  municip_code_current <- NULL
  # end

  if (x_year_end == 2019) {
    d <- fread(system.file("rawdata", "SYSVAK_2019-04-09-14-17.csv", package = "fhidata"), encoding = "UTF-8")
    norway_locs <- gen_norway_locations_long(x_year_end = x_year_end)$location_code
    norway_locs <- norway_locs[!norway_locs %in% "norway"]
  } else if (x_year_end == 2020) {
    d <- fread(system.file("rawdata", "SYSVAK_2019-04-09-14-17.csv", package = "fhidata"), encoding = "UTF-8")
    norway_locs <- gen_norway_locations_long(x_year_end = x_year_end)$location_code
    norway_locs <- norway_locs[!norway_locs %in% "norway"]
  }
  merging_municip <- gen_norway_municip_merging(x_year_end = x_year_end, x_year_start = 2019)
  merging_county <- gen_norway_county_merging(x_year_end = x_year_end, x_year_start = 2019)

  d[GEO == 0, location_code := "norge"]
  d[GEO > 0 & GEO < 100, location_code := glue::glue("county{X}", X = formatC(GEO, width = 2, flag = "0"))]
  d[GEO >= 100, location_code := glue::glue("municip{X}", X = formatC(GEO, width = 4, flag = "0"))]
  d[GEO >= 10000, location_code := glue::glue("district{X}", X = formatC(GEO, width = 6, flag = "0"))]

  d[, year := as.numeric(stringr::str_extract(AAR, "^[0-9][0-9][0-9][0-9]")) + 2]
  d <- d[SPVFLAGG == 0 & ALDER == "16_16" & !stringr::str_detect(location_code, "district")]
  d[, year_merging := 2019]

  dnorge <- d[stringr::str_detect(location_code, "norge")]
  dcounty <- d[stringr::str_detect(location_code, "county")]
  dmunicip <- d[stringr::str_detect(location_code, "municip")]

  dcounty <- merge(
    dcounty,
    merging_county[, c("county_code_current", "county_code_original", "year", "weighting")],
    by.x = c("location_code", "year_merging"),
    by.y = c("county_code_original", "year")
  )
  dcounty <- dcounty[, .(
    RATE = sum(RATE * weighting) / sum(weighting)
  ), keyby = .(
    county_code_current,
    KJONN,
    ALDER,
    VAKSINE,
    year
  )]
  setnames(dcounty, "county_code_current", "location_code")

  dmunicip <- merge(
    dmunicip,
    merging_municip[, c("municip_code_current", "municip_code_original", "year", "weighting")],
    by.x = c("location_code", "year_merging"),
    by.y = c("municip_code_original", "year")
  )
  dmunicip <- dmunicip[, .(
    RATE = sum(RATE * weighting) / sum(weighting)
  ), keyby = .(
    municip_code_current,
    KJONN,
    ALDER,
    VAKSINE,
    year
  )]
  setnames(dmunicip, "municip_code_current", "location_code")

  d <- rbind(dnorge, dcounty, dmunicip, fill = T)

  d[, age := 16]
  d[, proportion := RATE / 100]
  d[, vax := as.character(forcats::fct_recode(VAKSINE,
    "measles" = "Meslinger",
    "diphtheria" = "Difteri",
    "hpv" = "HPV",
    "pertussis" = "Kikhoste",
    "mumps" = "Kusma",
    "poliomyelitis" = "Poliomyelitt",
    "rubella" = "Rodehunder",
    "tetanus" = "Stivkrampe"
  ))]
  d <- d[, c("location_code", "year", "age", "vax", "proportion")]
  d[, imputed := FALSE]
  national_results <- d[location_code == "norge", .(
    national = mean(proportion)
  ),
  keyby = .(year, vax)
  ]
  skeleton <- data.table(expand.grid(
    location_code = norway_locs,
    year = unique(d$year),
    vax = unique(d$vax),
    stringsAsFactors = F
  ))
  d <- merge(d, skeleton,
    by = c("location_code", "year", "vax"), all = T
  )
  d <- merge(d, national_results, by = c("year", "vax"))
  d[is.na(age), age := 16]
  d[is.na(proportion), imputed := TRUE]
  d[is.na(proportion), proportion := national]
  d[, national := NULL]

  setcolorder(d, c("year", "location_code", "age", "vax", "proportion", "imputed"))

  return(invisible(d))
}
