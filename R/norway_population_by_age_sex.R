#' Population in Norway (2020 borders).
#'
#' We conveniently package population data taken from Statistics Norway.
#' This data is licensed under the Norwegian License for
#' Open Government Data (NLOD) 2.0.
#'
#' This dataset contains national/county/municipality/ward (city district) level population data
#' for every age (0 to 105 years old). The national level data is from year 1846, while all the
#' other levels have data from 2005.
#'
#' The counties and municipalities are updated for the 2020 borders.
#'
#' @format
#' \describe{
#' \item{calyear}{Calendar Year.}
#' \item{location_code}{The location code.}
#' \item{granularity_geo}{National/County/Municipality/BAregion.}
#' \item{age}{1 year ages from 0 to 105.}
#' \item{sex}{male/female}
#' \item{pop}{Number of people.}
#' \item{imputed}{FALSE if real data. TRUE if it is the last real data point carried forward.}
#' }
#' @source \url{https://www.ssb.no/en/statbank/table/07459/tableViewLayout1/}
"norway_population_by_age_sex_b2020"

norway_population_by_age_sex_b2020_cats <- function(cats=NULL){
  stopifnot(is.list(cats) | is.null(cats))

  d <- copy(fhidata::norway_population_by_age_sex_b2020)
  if(is.null(cats)){
    d[, age_cat := age]
  } else {
    for(i in seq_along(cats)){
      vals <- cats[[i]]
      name <- names(cats)[[i]]
      if(is.null(name)){
        name <- paste0(formatC(vals[1],width=2,flag="0"),"-",formatC(vals[length(vals)],width=2,flag="0"))
      } else if(name==""){
        name <- paste0(formatC(vals[1],width=2,flag="0"),"-",formatC(vals[length(vals)],width=2,flag="0"))
      }
      d[age %in% vals, age_cat := name]
    }
  }
  d <- d[!is.na(age_cat),.(
    pop = sum(pop)
  ),keyby=.(
    calyear, location_code, age_cat, sex, imputed, granularity_geo
  )]
  setnames(d, "age_cat", "age")
  setcolorder(d, c("calyear", "location_code", "granularity_geo", "age", "sex", "pop", "imputed"))

  return(d)
}

#' norway_population_by_age_sex_cats
#'
#' A function that easily categorizes the populations for you
#' @param cats A list containing vectors that you want to categorize
#' @param border The border year
#' @examples
#' norway_population_by_age_sex_cats(cats = list(c(1:10), c(11:20)))
#' norway_population_by_age_sex_cats(cats = list("one to ten" = c(1:10), "eleven to twenty" = c(11:20)))
#' norway_population_by_age_sex_cats(cats = list(c(1:10), c(11:20), "21+"=c(21:200)))
#' @export
norway_population_by_age_sex_cats <- function(cats=NULL, border = fhidata::config$border){
  stopifnot(border == 2020)
  if(border==2020){
    norway_population_by_age_sex_b2020_cats(cats = cats)
  }
}


# Creates the population dataset
# https://www.ssb.no/en/statbank/table/07459/tableViewLayout1/
# https://www.ssb.no/en/statbank/table/10826/ for wards
#' @import data.table
gen_norway_population_by_age_sex <- function(x_year_end, norway_locations_hierarchy_municip) {
  # norway_locations_hierarchy_municip = fhidata::norway_locations_municip_b2020
  stopifnot(x_year_end==2020)

  # x_year_end <- 2020
  # variables used in data.table functions in this function
  . <- NULL
  value <- NULL
  age <- NULL
  Var2 <- NULL
  agecont <- NULL
  pop <- NULL
  municip_code <- NULL
  municip_code_current <- NULL
  year_end <- NULL
  level <- NULL
  region <- NULL
  variable <- NULL
  agenum <- NULL
  imputed <- NULL
  county_code <- NULL
  municip_code_end <- NULL
  sex <- NULL
  contents <- NULL
  x <- NULL
  # end

  # municip and ward ----
  popFiles <- c(
    "norway_population_by_age_sex_2020.csv",
    "norway_population_by_age_sex_2021.csv"
  )
  pop <- vector("list", length = length(popFiles))
  for (i in seq_along(pop)) {
    pop[[i]] <- fread(system.file("rawdata", "population", popFiles[i], package = "fhidata"), encoding = "UTF-8")
    pop[[i]] <- melt.data.table(pop[[i]], id.vars = c("region", "age", "sex"))
  }
  pop <- rbindlist(pop)
  pop[, region := stringr::str_remove(region, "^K-")]
  pop[, location_code := sprintf("municip%s", stringr::str_extract(region, "^[0-9][0-9][0-9][0-9]"))]
  # correctly identify ward/bydels

  pop[, year := as.numeric(stringr::str_extract(variable, "[0-9][0-9][0-9][0-9]$"))]
  pop[, agenum := as.numeric(stringr::str_extract(age, "^[0-9]*"))]
  pop[, age := NULL]
  setnames(pop, "agenum", "age")

  pop <- pop[location_code != "municipNA"]
  pop[,
    sex := dplyr::case_when(
      sex=="Males" ~ "male",
      sex=="Females" ~ "female"
    )
  ]

  pop[
    norway_locations_hierarchy_municip,
    on=c("location_code==municip_code"),
    county_code := county_code
  ]

  pop[
    norway_locations_hierarchy_municip,
    on=c("location_code==municip_code"),
    baregion_code := baregion_code
  ]

  pop_municip <- copy(pop[,.(
    year,
    location_code,
    age,
    sex,
    value
  )])
  pop_county <- copy(pop[,.(
    year,
    location_code=county_code,
    age,
    sex,
    value
  )])
  pop_baregion <- copy(pop[,.(
    year,
    location_code=baregion_code,
    age,
    sex,
    value
  )])
  pop_nation <- copy(pop[,.(
    year,
    location_code="norge",
    age,
    sex,
    value
  )])

  pop <- rbind(
    pop_municip,
    pop_county,
    pop_baregion,
    pop_nation
  )
  pop <- pop[!is.na(location_code)]
  pop[, imputed := FALSE]

  # so far 2005 to 2020
  # year, municip_code, age, imputed, pop
  # imputing the future (2 years+)
  missingYears <- max(pop$year):(lubridate::year(lubridate::today()) + 2)
  if (length(missingYears) > 1) {
    copiedYears <- vector("list", length = length(missingYears) - 1)
    for (i in seq_along(copiedYears)) {
      copiedYears[[i]] <- pop[year == missingYears[1]]
      copiedYears[[i]][, year := year + i]
    }
    copiedYears <- rbindlist(copiedYears)
    copiedYears[, imputed := TRUE]
    pop <- rbind(pop, copiedYears)
  }

  pop[, granularity_geo := stringr::str_extract(location_code, "^[a-z]+")]
  pop[granularity_geo=="norge", granularity_geo := "nation"]
  setnames(pop, "value", "pop")

  setnames(pop, "year", "calyear")

  return(invisible(pop))
}


