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
  save(norway_ward_merging_b2020, file = file.path(base_loc, "norway_ward_merging_b2020.rda"), compress = "bzip2")

  # norway_fixing_merged_municips
  norway_fixing_merged_municips <- gen_norway_fixing_merged_municips(x_year_end = 2020)
  save(norway_fixing_merged_municips, file = file.path(base_loc, "norway_fixing_merged_municips.rda"), compress = "bzip2")

  # norway_municip_merging
  norway_municip_merging_b2020 <- gen_norway_municip_merging(x_year_end = 2020)
  save(norway_municip_merging_b2020, file = file.path(base_loc, "norway_municip_merging_b2020.rda"), compress = "bzip2")

  norway_municip_merging_b2019 <- gen_norway_municip_merging(x_year_end = 2019)
  save(norway_municip_merging_b2019, file = file.path(base_loc, "norway_municip_merging_b2019.rda"), compress = "bzip2")

  # norway_county_merging
  norway_county_merging_b2020 <- gen_norway_county_merging(x_year_end = 2020)
  save(norway_county_merging_b2020, file = file.path(base_loc, "norway_county_merging_b2020.rda"), compress = "bzip2")

  # norway_locations_ward
  norway_locations_ward_b2020 <- gen_norway_locations_ward(x_year_end = 2020)
  save(norway_locations_ward_b2020, file = file.path(base_loc, "norway_locations_ward_b2020.rda"), compress = "bzip2")

  # norway_locations
  norway_locations_b2020 <- gen_norway_locations(x_year_end = 2020)
  save(norway_locations_b2020, file = file.path(base_loc, "norway_locations_b2020.rda"), compress = "bzip2")

  # norway_locations_long
  norway_locations_long_b2020 <- gen_norway_locations_long(x_year_end = 2020)
  save(norway_locations_long_b2020, file = file.path(base_loc, "norway_locations_long_b2020.rda"), compress = "bzip2")

  # norway_population
  norway_population_b2020 <- gen_norway_population(x_year_end = 2020)
  save(norway_population_b2020, file = file.path(base_loc, "norway_population_b2020.rda"), compress = "xz")

  # norway_childhood_vax
  norway_childhood_vax_b2020 <- gen_norway_childhood_vax(x_year_end = 2020)
  save(norway_childhood_vax_b2020, file = file.path(base_loc, "norway_childhood_vax_b2020.rda"), compress = "xz")

  # other
  countries_nb_to_en <- gen_countries_nb_to_en()
  save(countries_nb_to_en, file = file.path(base_loc, "countries_nb_to_en.rda"))

  norway_dates_holidays <- gen_norway_dates_holidays()
  save(norway_dates_holidays, file = file.path(base_loc, "norway_dates_holidays.rda"), compress = "xz")

  # nordic data - locations long
  denmark_locations_long_b2020 <- gen_data_denmark_locations_long(x_year_end = 2020)
  save(denmark_locations_long_b2020, file = file.path(base_loc, "denmark_locations_long_b2020.rda"), compress = "xz")

  sweden_locations_long_b2020 <- gen_data_sweden_locations_long(x_year_end = 2020)
  save(sweden_locations_long_b2020, file = file.path(base_loc, "sweden_locations_long_b2020.rda"), compress = "xz")

  finland_locations_long_b2020 <- gen_data_finland_locations_long(x_year_end = 2020)
  save(finland_locations_long_b2020, file = file.path(base_loc, "finland_locations_long_b2020.rda"), compress = "xz")

  iceland_locations_long_b2020 <- gen_data_iceland_locations_long(x_year_end = 2020)
  save(iceland_locations_long_b2020, file = file.path(base_loc, "iceland_locations_long_b2020.rda"), compress = "xz")

  # nordic data - population
  population_denmark_b2020 <- gen_data_denmark_population(x_year_end = 2020)
  save(population_denmark_b2020, file = file.path(base_loc, "population_denmark_b2020.rda"), compress = "xz")

  population_sweden_b2020 <- gen_data_sweden_population(x_year_end = 2020)
  save(population_sweden_b2020, file = file.path(base_loc, "population_sweden_b2020.rda"), compress = "xz")

  population_finland_b2020 <- gen_data_finland_population(x_year_end = 2020)
  save(population_finland_b2020, file = file.path(base_loc, "population_finland_b2020.rda"), compress = "xz")

  population_iceland_b2020 <- gen_data_iceland_population(x_year_end = 2020)
  save(population_iceland_b2020, file = file.path(base_loc, "population_iceland_b2020.rda"), compress = "xz")

  # map stuff -- move to fhimaps

  norway_map_counties_label_positions_b2017 <- gen_norway_map_counties_label_positions(x_year_end = 2017)
  save(norway_map_counties_label_positions_b2017, file = file.path(base_loc, "norway_map_counties_label_positions_b2017.rda"), compress = "xz")
  norway_map_counties_label_positions_b2019 <- gen_norway_map_counties_label_positions(x_year_end = 2019)
  save(norway_map_counties_label_positions_b2019, file = file.path(base_loc, "norway_map_counties_label_positions_b2019.rda"), compress = "xz")
  norway_map_counties_label_positions_b2020 <- gen_norway_map_counties_label_positions(x_year_end = 2020)
  save(norway_map_counties_label_positions_b2020, file = file.path(base_loc, "norway_map_counties_label_positions_b2020.rda"), compress = "xz")

  norway_map_insert_title_position_b2017 <- gen_norway_map_insert_title_position(x_year_end = 2017)
  save(norway_map_insert_title_position_b2017, file = file.path(base_loc, "norway_map_insert_title_position_b2017.rda"), compress = "xz")
  norway_map_insert_title_position_b2019 <- gen_norway_map_insert_title_position(x_year_end = 2019)
  save(norway_map_insert_title_position_b2019, file = file.path(base_loc, "norway_map_insert_title_position_b2019.rda"), compress = "xz")
  norway_map_insert_title_position_b2020 <- gen_norway_map_insert_title_position(x_year_end = 2020)
  save(norway_map_insert_title_position_b2020, file = file.path(base_loc, "norway_map_insert_title_position_b2020.rda"), compress = "xz")


  # icpc_code ----
  icpc2_code_nb_2020 <- gen_data_icpc2_code_nb()
  save(icpc2_code_nb_2020, file = file.path(base_loc, "icpc2_code_nb_2020.rda"), compress = "xz")

  icpc2_code_en_2018 <- gen_data_icpc2_code_en()
  save(icpc2_code_en_2018, file = file.path(base_loc, "icpc2_code_en_2018.rda"), compress = "xz")


  # world_map <- gen_world_map()
  # save(world_map, file=file.path("/git","fhidata","data","world_map.rda"), compress = "xz")

  # europe_map_nuts0 <- gen_europe_map_nuts(nuts_level=0)
  # save(europe_map_nuts0, file=file.path("/git","fhidata","data","europe_map_nuts0.rda"), compress = "xz")

  # europe_map_nuts1 <- gen_europe_map_nuts(nuts_level=1)
  # save(europe_map_nuts1, file=file.path("/git","fhidata","data","europe_map_nuts1.rda"), compress = "xz")

  # europe_map_nuts2 <- gen_europe_map_nuts(nuts_level=2)
  # save(europe_map_nuts2, file=file.path("/git","fhidata","data","europe_map_nuts2.rda"), compress = "xz")

  # europe_map_nuts3 <- gen_europe_map_nuts(nuts_level=3)
  # save(europe_map_nuts3, file=file.path("/git","fhidata","data","europe_map_nuts3.rda"), compress = "xz")

  # norway_map_counties_b2017 <- gen_norway_map_counties(x_year_end=2017)
  # save(norway_map_counties_b2017, file=file.path("/git","/fhidata","data","norway_map_counties_b2017.rda"), compress = "xz")

  # norway_map_counties_with_insert_b2017 <- gen_norway_map_counties(x_year_end=2017, insert = T)
  # save(norway_map_counties_with_insert_b2017, file=file.path("/git","/fhidata","data","norway_map_counties_with_insert_b2017.rda"), compress = "xz")

  # norway_map_counties_b2019 <- gen_norway_map_counties(x_year_end=2019)
  # save(norway_map_counties_b2019, file=file.path("/git","/fhidata","data","norway_map_counties_b2019.rda"), compress = "xz")
  # norway_map_municips_b2019 <-  gen_norway_map_municips(x_year_end=2019)
  # save(norway_map_municips_b2019, file=file.path("/git","/fhidata","data","norway_map_municips_b2019.rda"), compress = "xz")

  # norway_map_counties_with_insert_b2019 <- gen_norway_map_counties(x_year_end=2019, insert = T)
  # save(norway_map_counties_with_insert_b2019, file=file.path("/git","/fhidata","data","norway_map_counties_with_insert_b2019.rda"), compress = "xz")
  # norway_map_municips_with_insert_b2019 <-  gen_norway_map_municips(x_year_end=2019, insert = T)
  # save(norway_map_municips_with_insert_b2019, file=file.path("/git","/fhidata","data","norway_map_municips_with_insert_b2019.rda"), compress = "xz")

  # norway_map_split_counties_b2020 <- gen_norway_map_counties(x_year_end=2020, split=T)
  # save(norway_map_split_counties_b2020, file=file.path("/git","/fhidata","data","norway_map_split_counties_b2020.rda"), compress = "xz")
  # norway_map_split_municips_b2020 <-  gen_norway_map_municips(x_year_end=2020, split=T)
  # save(norway_map_split_municips_b2020, file=file.path("/git","/fhidata","data","norway_map_split_municips_b2020.rda"), compress = "xz")

  # norway_map_counties_b2020 <- gen_norway_map_counties(x_year_end=2020)
  # save(norway_map_counties_b2020, file=file.path("/git","/fhidata","data","norway_map_counties_b2020.rda"), compress = "xz")
  # norway_map_municips_b2020 <-  gen_norway_map_municips(x_year_end=2020)
  # save(norway_map_municips_b2020, file=file.path("/git","/fhidata","data","norway_map_municips_b2020.rda"), compress = "xz")

  # norway_map_counties_with_insert_b2020 <- gen_norway_map_counties(x_year_end=2020, insert = T)
  # save(norway_map_counties_with_insert_b2020, file=file.path("/git","/fhidata","data","norway_map_counties_with_insert_b2020.rda"), compress = "xz")
  # norway_map_municips_with_insert_b2020 <-  gen_norway_map_municips(x_year_end=2020, insert = T)
  # save(norway_map_municips_with_insert_b2020, file=file.path("/git","/fhidata","data","norway_map_municips_with_insert_b2020.rda"), compress = "xz")

  # load(file.path(base_loc,"norway_locations_b2019.rda"))
  # load(file.path(base_loc,"norway_map_municips_b2019.rda"))
  # senorge_b2019 <- gen_senorge(norway_locations_b2019, norway_map_municips_b2019)
  # save(senorge_b2019, file=file.path(base_loc,"senorge_b2019.rda"), compress = "xz")

  # load(file.path(base_loc,"norway_locations_b2020.rda"))
  # load(file.path(base_loc,"norway_map_municips_b2020.rda"))
  # senorge_b2020 <- gen_senorge(norway_locations_b2020, norway_map_municips_b2020)
  # save(senorge_b2020, file=file.path(base_loc,"senorge_b2020.rda"), compress = "xz")
}
