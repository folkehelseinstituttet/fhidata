addin_make_skeleton <- function(){
  require_namespace("rstudioapi")
  rstudioapi::insertText(
    '
# the data is loaded in
data <- copy(d_agg)

# 1. Create a variable (possibly a list) to hold the data
d_agg <- list()
d_agg$day_municip <- copy(data$day_municip)
d_agg$day_county <- copy(data$day_county)
d_agg$day_nation <- copy(data$day_nation)

# 2. Clean the data

# fixing location codes
# 3. Replace NAs as appropriate for MSIS location numbers
d_agg$day_municip[is.na(location_msis_municip), location_msis_municip := 9999]
d_agg$day_county[is.na(location_msis_county), location_msis_county := 99]

# 4. Convert MSIS location numbers to location_code"s using `fhidata::norway_locations_msis_to_fhidata`
# you may also do redistricting (kommunesammenslaaing) at this point (fhidata::norway_locations_redistricting())
d_agg$day_municip[, location_code_original := fhidata::norway_locations_msis_to_fhidata(location_msis_municip)]
d_agg$day_county[, location_code_original := fhidata::norway_locations_msis_to_fhidata(location_msis_county)]
d_agg$day_nation[, location_code_original := fhidata::norway_locations_msis_to_fhidata(location_msis_nation)]

# redistricting
for(d_temp in d_agg){
  d_temp[, calyear := lubridate::year(date)]
  d_temp[
    fhidata::norway_locations_redistricting(),
    on = c("location_code_original", "calyear"),
    c("location_code_current", "weighting") := .(location_code_current, weighting)
  ]
}

# 5. Re-aggregate your data to different geographical levels to ensure that duplicates have now been removed
# this will also fix the redistricting/kommunesammenslaaing issues
for(i in seq_along(d_agg)){
  d_agg[[i]] <- d_agg[[i]][,.(
    cases_n = round(sum(cases_n*weighting))
  ), keyby=.(
    location_code = location_code_current,
    date
  )]
}

d_agg[]

# 6. Pull out important dates
date_min <- min(d_agg$day_nation$date)
date_max <- max(d_agg$day_nation$date)

# 7. Create `multiskeleton`
# granularity_geo should have the following groups:
# - nodata (when no data is available, and there is no "finer" data available to aggregate up)
# - all levels of granularity_geo where you have data available
# If you do not have data for a specific granularity_geo, but there is "finer" data available
# then you should not include this granularity_geo in the multiskeleton, because you will create
# it later when you aggregate up your data (baregion)
multiskeleton_day <- fhidata::make_skeleton(
  date_min = date_min,
  date_max = date_max,
  granularity_geo = list(
    "nodata" = c(
      "wardoslo",
      "extrawardoslo",
      "missingwardoslo",
      "wardbergen",
      "missingwardbergen",
      "wardstavanger",
      "missingwardstavanger"
    ),

    "municip" = c(
      "municip",
      "notmainlandmunicip",
      "missingmunicip"
    ),

    "county" = c(
      "county",
      "notmainlandcounty",
      "missingcounty"
    ),

    "nation" = c(
      "nation"
    )
  )
)

# 8. Merge in the information you have at different geographical granularities
# one level at a time
# municip
multiskeleton_day$municip[
  d_agg$day_municip,
  on = c("location_code", "date"),
  cases_n := cases_n
]
multiskeleton_day$municip[is.na(cases_n), cases_n := 0]

multiskeleton_day$municip[]

# county
multiskeleton_day$county[
  d_agg$day_county,
  on = c("location_code", "date"),
  cases_n := cases_n
]
multiskeleton_day$county[is.na(cases_n), cases_n := 0]

multiskeleton_day$county[]

# nation
multiskeleton_day$nation[
  d_agg$day_nation,
  on = c("location_code", "date"),
  cases_n := cases_n
]
multiskeleton_day$nation[is.na(cases_n), cases_n := 0]

multiskeleton_day$nation[]

# 9. Aggregate up to higher geographical granularities
multiskeleton_day$baregion <- multiskeleton_day$municip[
  fhidata::norway_locations_hierarchy(
    from = "municip",
    to = "baregion"
  ),
  on = c(
    "location_code==from_code"
  )
][,
  .(
    cases_n = sum(cases_n),
    granularity_geo = "baregion"
  ),
  by=.(
    granularity_time,
    date,
    location_code = to_code
  )
]

multiskeleton_day$baregion[]

# combine all the different granularity_geos
skeleton_day <- rbindlist(multiskeleton_day, fill = TRUE, use.names = TRUE)

skeleton_day[]

# 10. (If desirable) aggregate up to higher time granularities
# if necessary, it is now easy to aggregate up to weekly data from here
skeleton_week <- copy(skeleton_day)
skeleton_week[, isoyearweek := fhiplot::isoyearweek_c(date)]
skeleton_week <- skeleton_week[
  ,
  .(
    cases_n = sum(cases_n),
    granularity_time = "week"
  ),
  keyby = .(
    isoyearweek,
    granularity_geo,
    location_code
  )
]

skeleton_week[]
'
  )
}
