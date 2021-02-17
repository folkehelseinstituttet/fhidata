# Creates the norway_locations, norway_municip_merging, and norway_population data.table
# @param base_loc Folder where data will be saved

gen_data_all <- function(base_loc) {
  # gen_data_all(file.path(getwd(),"data"))
  # base_loc = file.path(getwd(),"data")

  # gen_world_dates_isoyearweek ----
  world_dates_isoyearweek <- gen_days()
  save(world_dates_isoyearweek, file = file.path(base_loc, paste0("world_dates_isoyearweek",".rda")), compress = "xz")

  # norway_dates_holidays ----
  norway_dates_holidays <- gen_norway_dates_holidays()
  save(norway_dates_holidays, file = file.path(base_loc, paste0("norway_dates_holidays",".rda")), compress = "xz")

  # nordic ----
  denmark_locations_names_b2020 <- gen_denmark_locations_names(2020)
  save(denmark_locations_names_b2020, file = file.path(base_loc, paste0("denmark_locations_names_b2020",".rda")), compress = "xz")

  sweden_locations_names_b2020 <- gen_sweden_locations_names(2020)
  save(sweden_locations_names_b2020, file = file.path(base_loc, paste0("sweden_locations_names_b2020",".rda")), compress = "xz")

  finland_locations_names_b2020 <- gen_finland_locations_names(2020)
  save(finland_locations_names_b2020, file = file.path(base_loc, paste0("finland_locations_names_b2020",".rda")), compress = "xz")

  iceland_locations_names_b2020 <- gen_iceland_locations_names(2020)
  save(iceland_locations_names_b2020, file = file.path(base_loc, paste0("iceland_locations_names_b2020",".rda")), compress = "xz")

  denmark_population_by_age_b2020 <- gen_denmark_population_by_age(2020)
  save(denmark_population_by_age_b2020, file = file.path(base_loc, paste0("denmark_population_by_age_b2020",".rda")), compress = "xz")

  sweden_population_by_age_b2020 <- gen_sweden_population_by_age(2020)
  save(sweden_population_by_age_b2020, file = file.path(base_loc, paste0("sweden_population_by_age_b2020",".rda")), compress = "xz")

  finland_population_by_age_b2020 <- gen_finland_population_by_age(2020)
  save(finland_population_by_age_b2020, file = file.path(base_loc, paste0("finland_population_by_age_b2020",".rda")), compress = "xz")

  iceland_population_by_age_b2020 <- gen_iceland_population_by_age(2020)
  save(iceland_population_by_age_b2020, file = file.path(base_loc, paste0("iceland_population_by_age_b2020",".rda")), compress = "xz")

  # icpc_codes ----
  icpc2_codes <- gen_icpc2_codes()
  save(icpc2_codes, file = file.path(base_loc, paste0("icpc2_codes",".rda")), compress = "xz")

  # norway_childhood_vax
  norway_childhood_vax_b2020 <- gen_norway_childhood_vax(x_year_end = 2020)
  save(norway_childhood_vax_b2020, file = file.path(base_loc, paste0("norway_childhood_vax_b2020",".rda")), compress = "xz")

  # norway_locations_redistricting_all_b2020
  norway_locations_redistricting_county_b2020 <- gen_norway_locations_redistricting_county(2020)
  norway_locations_redistricting_municip_b2020 <- gen_norway_locations_redistricting_municip(2020)
  norway_locations_redistricting_ward_b2020 <- gen_norway_locations_redistricting_ward(2020)
  norway_locations_redistricting_notmainlandcounty_b2020 <- gen_norway_locations_redistricting_notmainlandcounty(2020)
  norway_locations_redistricting_notmainlandmunicip_b2020 <- gen_norway_locations_redistricting_notmainlandmunicip(2020)
  norway_locations_redistricting_missingcounty_b2020 <- gen_norway_locations_redistricting_missingcounty(2020)
  norway_locations_redistricting_missingmunicip_b2020 <- gen_norway_locations_redistricting_missingmunicip(2020)

  norway_locations_redistricting_b2020 <- rbind(
    data.table::data.table(
      location_code_current = "norge",
      location_code_original = "norge",
      year = 1975:(lubridate::year(lubridate::today())+10),
      weighting = 1
    ),
    norway_locations_redistricting_county_b2020,
    norway_locations_redistricting_municip_b2020,
    norway_locations_redistricting_ward_b2020,
    norway_locations_redistricting_notmainlandcounty_b2020,
    norway_locations_redistricting_notmainlandmunicip_b2020,
    norway_locations_redistricting_missingcounty_b2020,
    norway_locations_redistricting_missingmunicip_b2020
  )
  norway_locations_redistricting_b2020[, granularity_geo := get_granularity_geo(location_code_current)]
  save(norway_locations_redistricting_b2020, file = file.path(base_loc, paste0("norway_locations_redistricting_b2020",".rda")), compress = "xz")

  # norway_locations_hierarchy_all_b2020
  norway_locations_hierarchy_all_b2020 <- gen_norway_locations_hierarchy_all(2020)
  save(norway_locations_hierarchy_all_b2020, file = file.path(base_loc, paste0("norway_locations_hierarchy_all_b2020",".rda")), compress = "xz")

  # norway_locations_names_b2020
  norway_locations_names_b2020 <- gen_norway_locations_names(2020)
  save(norway_locations_names_b2020, file = file.path(base_loc, paste0("norway_locations_names_b2020",".rda")), compress = "xz")

  # norway_population_by_age_sex_b2020
  load(file.path(base_loc, "norway_locations_municip_b2020.rda"))
  norway_population_by_age_sex_b2020 <- gen_norway_population_by_age_sex(2020, norway_locations_municip_b2020)
  save(norway_population_by_age_sex_b2020, file = file.path(base_loc, paste0("norway_population_by_age_sex_b2020",".rda")), compress = "xz")

  # norway_population_by_age_b2020
  norway_population_by_age_b2020 <- gen_norway_population_by_age(2020)
  save(norway_population_by_age_b2020, file = file.path(base_loc, paste0("norway_population_by_age_b2020",".rda")), compress = "xz")

}





