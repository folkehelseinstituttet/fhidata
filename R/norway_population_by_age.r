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
#' \item{pop}{Number of people.}
#' \item{imputed}{FALSE if real data. TRUE if it is the last real data point carried forward.}
#' }
#' @source \url{https://www.ssb.no/en/statbank/table/07459/tableViewLayout1/}
"norway_population_by_age_b2020"

norway_population_by_age_b2020_cats <- function(cats=NULL){
  stopifnot(is.list(cats) | is.null(cats))

  d <- copy(fhidata::norway_population_by_age_b2020)
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

#' norway_population_by_age_cats
#'
#' A function that easily categorizes the populations for you
#' @param cats A list containing vectors that you want to categorize
#' @param border The border year
#' @examples
#' norway_population_by_age_cats(cats = list(c(1:10), c(11:20)))
#' norway_population_by_age_cats(cats = list("one to ten" = c(1:10), "eleven to twenty" = c(11:20)))
#' norway_population_by_age_cats(cats = list(c(1:10), c(11:20), "21+"=c(21:200)))
#' @export
norway_population_by_age_cats <- function(cats = NULL, border = fhidata::config$border){
  stopifnot(border == 2020)
  if(border==2020){
    norway_population_by_age_b2020_cats(cats = cats)
  }
}

# Creates the population dataset
# https://www.ssb.no/en/statbank/table/07459/tableViewLayout1/
# https://www.ssb.no/en/statbank/table/10826/ for wards
#' @import data.table
gen_norway_population_by_age <- function(x_year_end, original = FALSE) {

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
    "Personer2005-2009.csv",
    "Personer2010-2014.csv",
    "Personer2015-2018.csv",
    "Personer2019.csv",
    "Personer2020.csv",
    "Personer2021.csv",
    "Personerward2001-2020.csv",
    "Personerward2021.csv"
  )
  pop <- vector("list", length = length(popFiles))
  for (i in seq_along(pop)) {
    pop[[i]] <- fread(system.file("rawdata", "population", popFiles[i], package = "fhidata"), encoding = "UTF-8")
    pop[[i]] <- melt.data.table(pop[[i]], id.vars = c("region", "age"))
  }
  pop <- rbindlist(pop)
  pop[, region := stringr::str_remove(region, "^K-")]
  pop[, municip_code := sprintf("municip%s", stringr::str_extract(region, "^[0-9][0-9][0-9][0-9]"))]
  # correctly identify ward/bydels
  pop[, ward_code := sprintf("%s", stringr::str_extract(region, "^[0-9][0-9][0-9][0-9][0-9][0-9]"))]
  pop[, ward_prefix := ""]
  pop[ward_code!="NA" & municip_code %in% c("municip0301"), ward_prefix := "wardoslo"]
  pop[ward_code!="NA" & municip_code %in% c("municip1201", "municip4601"), ward_prefix := "wardbergen"]
  pop[ward_code!="NA" & municip_code %in% c("municip1103"), ward_prefix := "wardstavanger"]
  pop[ward_code!="NA" & municip_code %in% c("municip1601", "municip5001"), ward_prefix := "wardtrondheim"]
  pop[,ward_code := paste0(ward_prefix,ward_code)]
  pop[ward_code=="wardoslo030116", ward_code := "extrawardoslo030116"]
  pop[ward_code=="wardoslo030117", ward_code := "extrawardoslo030117"]
  pop[ward_code!="NA", municip_code := ward_code]

  pop[, year := as.numeric(stringr::str_extract(variable, "[0-9][0-9][0-9][0-9]$"))]
  pop[, agenum := as.numeric(stringr::str_extract(age, "^[0-9]*"))]
  pop[, age := NULL]
  setnames(pop, "agenum", "age")


  pop <- pop[municip_code != "municipNA"]
  pop <- pop[, .(
    pop = sum(value)
  ), keyby = .(
    municip_code, age, year
  )]

  # Fixing broken parts in the population data
  # part 1
  pop2 <- pop[municip_code == "municip0710" & year <= 2017]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip0706"]
  pop2[, pop := round(pop / 3)]
  pop <- rbind(pop, pop2)

  pop2 <- pop[municip_code == "municip0710" & year <= 2017]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip0719"]
  pop2[, pop := round(pop / 3)]
  pop <- rbind(pop, pop2)

  pop2 <- pop[municip_code == "municip0710" & year <= 2017]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip0720"]
  pop2[, pop := round(pop / 3)]
  pop <- rbind(pop, pop2)

  # part 2
  pop2 <- pop[municip_code == "municip1756" & year <= 2012]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip1723"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)

  pop2 <- pop[municip_code == "municip1756" & year <= 2012]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip1729"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)

  # part 3
  pop2 <- pop[municip_code == "municip5046" & year <= 2018]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip1901"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)

  pop2 <- pop[municip_code == "municip1756" & year <= 2018]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip1915"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)

  # part 4
  pop2 <- pop[municip_code == "municip1505" & year <= 2008]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip1503"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)

  pop2 <- pop[municip_code == "municip1505" & year <= 2008]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip_code := "municip1556"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)
  pop[, imputed := FALSE]




  if (original) {
    return(pop)
  }

  # kommunesammenslaing ----
  # x_year_end <- 2020
  merging_1 <- gen_norway_municip_merging(x_year_end = x_year_end)
  merging_2 <- gen_norway_ward_merging(x_year_end = x_year_end)
  setnames(merging_2, names(merging_1))
  norway_merging <- rbind(merging_1, merging_2)
  pop <- merge(
    pop,
    norway_merging[, c("year", "municip_code_current", "municip_code_original")],
    by.x = c("municip_code", "year"),
    by.y = c("municip_code_original", "year")
  )
  pop <- pop[, .(pop = sum(pop)),
    keyby = .(
      year,
      municip_code = municip_code_current,
      age,
      imputed
    )
  ]

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

  pop[, level := stringr::str_extract(municip_code, "^[a-z]+")]

  # making county pop data ----
  counties <- merge(
    pop,
    gen_norway_locations(x_year_end = x_year_end)[, c("municip_code", "county_code")],
    by = "municip_code"
  )

  check_ref_to_new(
    xref = unique(pop[level=="municip"]$municip_code),
    xnew = unique(counties$municip_code)
  )

  if (nrow(counties) != nrow(pop[level=="municip"])) {
    stop("nrow(counties) != nrow(pop)")
  }

  counties <- counties[, .(
    pop = sum(pop)
  ), keyby = .(
    year,
    municip_code = county_code,
    age,
    imputed
  )]
  counties[, level := "county"]

  # making baregion pop data ----
  if(x_year_end==2020){
    baregions <- merge(
      pop,
      gen_norway_locations(x_year_end = x_year_end)[, c("municip_code", "baregion_code")],
      by = "municip_code"
    )

    baregions <- baregions[!is.na(baregion_code), .(
      pop = sum(pop)
    ), keyby = .(
      year,
      municip_code = baregion_code,
      age,
      imputed
    )]
    baregions[, level := "baregion"]
  }

  # pull in full norwegian data ----
  norway <- data.table(utils::read.csv(url("https://data.ssb.no/api/v0/dataset/59322.csv?lang=en"), stringsAsFactors = FALSE))
  norway <- norway[sex == "0 Both sexes"]
  norway[, sex := NULL]
  norway[, contents := NULL]
  norway[, x := as.numeric(stringr::str_extract(age, "^[0-9][0-9][0-9]"))]
  norway[, age := NULL]
  setnames(norway, c("year", "pop", "age"))
  norway[, level := "nation"]
  norway[, municip_code := "norge"]
  norway[, imputed := FALSE]
  missingYearsNational <- (max(norway$year) + 1):(lubridate::year(lubridate::today()) + 2)
  for (i in missingYearsNational) {
    popx <- norway[year == max(year)]
    popx[, year := i]
    popx[, imputed := TRUE]
    norway <- rbind(norway, popx)
  }


  if(x_year_end==2020){
    pop <- rbind(norway, counties, pop, baregions)
  } else {
    pop <- rbind(norway, counties, pop)
  }

  #  svalbard + jan mayen ----
  # age: -99
  pop_svalbard_raw <- readxl::read_excel(system.file("rawdata", "population", "Personer_svalbard_1990-2020.xlsx", package = "fhidata"))
  # county21: all 4 rows
  # municip: everything apart from barentsburg
  pop_sv <- data.frame(t(pop_svalbard_raw[, -1]))
  years <- rownames(pop_sv)
  colnames(pop_sv) <- c('Longyearbyen_nyalesund1', 'Longyearbyen_nyalesund2', 'Barentsburg', 'Hornsund')

  setDT(pop_sv)
  pop_sv[, notmainlandcounty21 := rowSums(.SD, na.rm=T), .SDcols=colnames(pop_sv)]
  pop_sv[, notmainlandmunicip2100 := rowSums(.SD, na.rm=T), .SDcols=colnames(pop_sv)[c(1,2,4)]]
  pop_sv[, year := as.numeric(years)]
  pop_sv[, imputed := F]



  # add missing years
  if (length(missingYears) > 1) {
    copiedYears <- vector("list", length = length(missingYears) - 1)
    for (i in seq_along(copiedYears)) {
      copiedYears[[i]] <- pop_sv[year == missingYears[1]]
      copiedYears[[i]][, year := year + i]
    }
    copiedYears <- rbindlist(copiedYears)
    copiedYears[, imputed := TRUE]
    pop_sv <- rbind(pop_sv, copiedYears)
  }


  # jan mayen (county22)
  pop_jm <- data.table(year = unique(c(as.numeric(years), missingYears)),
                       notmainlandmunicip2200 = 26,
                       notmainlandcounty22 = 26,
                       imputed = F)
  pop_jm[year>lubridate::year(lubridate::today()), imputed := T]


  # separate county, municip
  pop_notmainlandcounty21 <- pop_sv[, .(year, pop = notmainlandcounty21, imputed)]
  pop_notmainlandcounty21[, municip_code := 'notmainlandcounty21']
  pop_notmainlandcounty21[, level := 'notmainlandcounty']

  pop_notmainlandmunicip2100 <- pop_sv[, .(year, pop = notmainlandmunicip2100, imputed)]
  pop_notmainlandmunicip2100[, municip_code := 'notmainlandmunicip2100']
  pop_notmainlandmunicip2100[, level := 'notmainlandmunicip']

  pop_notmainlandcounty22 <- pop_jm[, .(year, pop = notmainlandcounty22, imputed)]
  pop_notmainlandcounty22[, municip_code := 'notmainlandcounty22']
  pop_notmainlandcounty22[, level := 'notmainlandcounty']

  pop_notmainlandmunicip2200 <- pop_jm[, .(year, pop = notmainlandmunicip2200, imputed)]
  pop_notmainlandmunicip2200[, municip_code := 'notmainlandmunicip2200']
  pop_notmainlandmunicip2200[, level := 'notmainlandmunicip']

  # match year: from 2005 to 2022
  pop_notmainlandcounty21 <- pop_notmainlandcounty21[year>=2005]
  pop_notmainlandmunicip2100 <- pop_notmainlandmunicip2100[year>=2005]
  pop_notmainlandcounty22 <- pop_notmainlandcounty22[year>=2005]
  pop_notmainlandmunicip2200 <- pop_notmainlandmunicip2200[year>=2005]





  # ukjent ----
  # age: -99
  # pop: NA
  pop_county_unknown <- data.table(year = unique(pop$year),
                                   pop = NA_real_,
                                   municip_code = 'missingcounty99',
                                   level = 'missingcounty',
                                   imputed = F)

  pop_municip_unknown <- data.table(year = unique(pop$year),
                                   pop = NA_real_,
                                   municip_code = 'missingmunicip9999',
                                   level = 'missingmunicip',
                                   imputed = F)


  # combine svalbard and unknown, set age, force imputed T for greater than this year
  popx <- rbind(pop_notmainlandcounty21,
                pop_notmainlandmunicip2100,
                pop_notmainlandcounty22,
                pop_notmainlandmunicip2200,
                pop_county_unknown,
                pop_municip_unknown)
  popx[, age := -99]
  popx[, granularity_geo := level]
  popx[, level := NULL]
  popx[year>lubridate::year(lubridate::today()), imputed := T]
  popx[, sex := "total"]


  # final ----
  pop[, granularity_geo := level]
  pop[, level := NULL]
  pop[, sex := "total"]

  final_order <- c("year", "municip_code", "granularity_geo", "age", "sex", "pop", "imputed")
  setorderv(pop, final_order)
  setcolorder(pop, final_order)
  setcolorder(popx, final_order)

  # finally bind these two
  pop <- rbind(pop, popx)
  setnames(pop, "municip_code", "location_code")

  setnames(pop, "year", "calyear")

  return(invisible(pop))
}
