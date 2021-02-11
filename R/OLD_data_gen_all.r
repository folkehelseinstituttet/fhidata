# Creates the norway_locations, norway_municip_merging, and norway_population data.table
# @param base_loc Folder where data will be saved

gen_data_all <- function(base_loc) {
  # gen_data_all(file.path(getwd(),"data"))
  # base_loc = file.path(getwd(),"data")
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(base_loc)

  days <- gen_days()
  save(days, file = file.path(base_loc, "days.rda"), compress = "bzip2")

  # norway_ward_merging
  norway_ward_merging_b2020 <- gen_norway_ward_merging(x_year_end = 2020)
  cat('norway ward merging done\n')
  save(norway_ward_merging_b2020, file = file.path(base_loc, "norway_ward_merging_b2020.rda"), compress = "bzip2")

  # norway_fixing_merged_municips
  norway_fixing_merged_municips <- gen_norway_fixing_merged_municips(x_year_end = 2020)
  cat('norway fixing merged municips done\n')
  save(norway_fixing_merged_municips, file = file.path(base_loc, "norway_fixing_merged_municips.rda"), compress = "bzip2")

  # norway_municip_merging
  norway_municip_merging_b2020 <- gen_norway_municip_merging(x_year_end = 2020)
  cat('norway merged municips 2020 done\n')
  save(norway_municip_merging_b2020, file = file.path(base_loc, "norway_municip_merging_b2020.rda"), compress = "bzip2")

  norway_municip_merging_b2019 <- gen_norway_municip_merging(x_year_end = 2019)
  cat('norway merged municips 2019 done\n')
  save(norway_municip_merging_b2019, file = file.path(base_loc, "norway_municip_merging_b2019.rda"), compress = "bzip2")

  # norway_county_merging
  norway_county_merging_b2020 <- gen_norway_county_merging(x_year_end = 2020)
  cat('norway county merging done\n')
  save(norway_county_merging_b2020, file = file.path(base_loc, "norway_county_merging_b2020.rda"), compress = "bzip2")


  # norway_missingcounty_merging
  norway_missingcounty_merging_b2020 <- gen_norway_missingcounty_merging(x_year_end = 2020)
  cat('norway merged county(missing) 2020 done\n')
  save(norway_missingcounty_merging_b2020, file = file.path(base_loc, "norway_missingcounty_merging_b2020.rda"), compress = "bzip2")


  # norway_missingmunicip_merging
  norway_missingmunicip_merging_b2020 <- gen_norway_missingmunicip_merging(x_year_end = 2020)
  cat('norway merged municips(missing) 2020 done\n')
  save(norway_missingmunicip_merging_b2020, file = file.path(base_loc, "norway_missingmunicip_merging_b2020.rda"), compress = "bzip2")


  # norway_notmainlandcounty_merging
  norway_notmainlandcounty_merging_b2020 <- gen_norway_notmainlandcounty_merging(x_year_end = 2020)
  cat('norway merged county(not mainland) 2020 done\n')
  save(norway_notmainlandcounty_merging_b2020, file = file.path(base_loc, "norway_notmainlandcounty_merging_b2020.rda"), compress = "bzip2")

  # norway_notmainlandmunicip_merging
  norway_notmainlandmunicip_merging_b2020 <- gen_norway_notmainlandmunicip_merging(x_year_end = 2020)
  cat('norway merged county(not mainland) 2020 done\n')
  save(norway_notmainlandmunicip_merging_b2020, file = file.path(base_loc, "norway_notmainlandmunicip_merging_b2020.rda"), compress = "bzip2")


  # norway_locations
  norway_locations_b2020 <- gen_norway_locations(x_year_end = 2020)
  cat('norway locations done\n')
  save(norway_locations_b2020, file = file.path(base_loc, "norway_locations_b2020.rda"), compress = "bzip2")

  # norway_locations_ward
  norway_locations_ward_b2020 <- gen_norway_locations_ward(x_year_end = 2020)
  cat('norway locations ward done\n')
  save(norway_locations_ward_b2020, file = file.path(base_loc, "norway_locations_ward_b2020.rda"), compress = "bzip2")

  # norway_locations_municip_b2020 (the same as norway_locations_b2020)
  norway_locations_municip_b2020 <- gen_norway_locations_municip(x_year_end = 2020)
  cat('norway locations municip done\n')
  save(norway_locations_municip_b2020, file = file.path(base_loc, "norway_locations_municip_b2020.rda"), compress = "bzip2")


  # norway_locations_notmainland
  norway_locations_notmainland_b2020 <- gen_norway_locations_notmainland(x_year_end = 2020)
  cat('norway locations not mainland done\n')
  save(norway_locations_notmainland_b2020, file = file.path(base_loc, "norway_locations_notmainland_b2020_b2020.rda"), compress = "bzip2")


  # norway_locations_missing
  norway_locations_missing_b2020 <- gen_norway_locations_missing(x_year_end = 2020)
  cat('norway locations missing done\n')
  save(norway_locations_missing_b2020, file = file.path(base_loc, "norway_locations_missing_b2020.rda"), compress = "bzip2")


  # norway_locations_long
  norway_locations_long_b2020 <- gen_norway_locations_long(x_year_end = 2020)
  cat('norway locations long done\n')
  save(norway_locations_long_b2020, file = file.path(base_loc, "norway_locations_long_b2020.rda"), compress = "bzip2")

  # norway_population
  norway_population_b2020 <- gen_norway_population(x_year_end = 2020)
  cat('norway population done\n')
  save(norway_population_b2020, file = file.path(base_loc, "norway_population_b2020.rda"), compress = "xz")

  # norway_childhood_vax
  norway_childhood_vax_b2020 <- gen_norway_childhood_vax(x_year_end = 2020)
  cat('norway childhood vax done\n')
  save(norway_childhood_vax_b2020, file = file.path(base_loc, "norway_childhood_vax_b2020.rda"), compress = "xz")

  # other
  countries_nb_to_en <- gen_countries_nb_to_en()
  save(countries_nb_to_en, file = file.path(base_loc, "countries_nb_to_en.rda"))

  norway_dates_holidays <- gen_norway_dates_holidays()
  save(norway_dates_holidays, file = file.path(base_loc, "norway_dates_holidays.rda"), compress = "xz")

  # nordic data - locations long
  denmark_locations_long_b2020 <- gen_data_denmark_locations_long(x_year_end = 2020)
  cat('denmark locations done\n')
  save(denmark_locations_long_b2020, file = file.path(base_loc, "denmark_locations_long_b2020.rda"), compress = "xz")

  sweden_locations_long_b2020 <- gen_data_sweden_locations_long(x_year_end = 2020)
  cat('sweden locations done\n')
  save(sweden_locations_long_b2020, file = file.path(base_loc, "sweden_locations_long_b2020.rda"), compress = "xz")

  finland_locations_long_b2020 <- gen_data_finland_locations_long(x_year_end = 2020)
  cat('finland locations done\n')
  save(finland_locations_long_b2020, file = file.path(base_loc, "finland_locations_long_b2020.rda"), compress = "xz")

  iceland_locations_long_b2020 <- gen_data_iceland_locations_long(x_year_end = 2020)
  cat('iceland locations done\n')
  save(iceland_locations_long_b2020, file = file.path(base_loc, "iceland_locations_long_b2020.rda"), compress = "xz")

  # nordic data - population
  population_denmark_b2020 <- gen_data_denmark_population(x_year_end = 2020)
  cat('denmark population done\n')
  save(population_denmark_b2020, file = file.path(base_loc, "population_denmark_b2020.rda"), compress = "xz")

  population_sweden_b2020 <- gen_data_sweden_population(x_year_end = 2020)
  cat('sweden population done\n')
  save(population_sweden_b2020, file = file.path(base_loc, "population_sweden_b2020.rda"), compress = "xz")

  population_finland_b2020 <- gen_data_finland_population(x_year_end = 2020)
  cat('finland population done\n')
  save(population_finland_b2020, file = file.path(base_loc, "population_finland_b2020.rda"), compress = "xz")

  population_iceland_b2020 <- gen_data_iceland_population(x_year_end = 2020)
  cat('iceland population done\n')
  save(population_iceland_b2020, file = file.path(base_loc, "population_iceland_b2020.rda"), compress = "xz")

  # icpc_code ----
  icpc2_code <- gen_data_icpc2_code()
  save(icpc2_code, file = file.path(base_loc, "icpc2_code.rda"), compress = "xz")
}
