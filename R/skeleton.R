#' make_skeleton
#'
#' The easiest way to make structural data skeletons.
#' @param date_min The minimum date for the skeleton
#' @param date_max The maximum date for the skeleton
#' @param yrwk_min The minimum yrwk for the skeleton
#' @param yrwk_max The maximum yrwk for the skeleton
#' @param time_total Producing a 'total' time for the skeleton
#' @param location_code The location_code's wanted for the skeleton
#' @param granularity_geo The granularity_geo's wanted for the skeleton. If this is a list, then the returned value will also be in a list (see examples).
#' @param location_reference A data.table that contains two columns: location_code and granularity_geo
#' @param ... Other variables to include in the skeleton
#' @examples
#' make_skeleton(date_min="2020-01-01", date_max="2020-01-30", granularity_geo = c("nation", "county"))[]
#' make_skeleton(yrwk_min="2020-01", yrwk_max="2020-15", granularity_geo = c("nation", "county"))[]
#' make_skeleton(time_total = TRUE, granularity_geo = c("nation", "wardoslo"))[]
#' make_skeleton(time_total = TRUE, granularity_geo = list(c("wardoslo", "missingwardoslo"), c("county", "missingcounty", "notmainlandcounty"), "nation"))[]
#' make_skeleton(time_total = TRUE, granularity_geo = list("firstbatch" = c("wardoslo", "missingwardoslo"), c("county", "missingcounty", "notmainlandcounty"), "nation"))[]
#' make_skeleton(yrwk_min = "2020-01", yrwk_max = "2020-03", time_total = TRUE, granularity_geo = c("nation", "wardoslo"))[]
#' @export
make_skeleton <- function(
date_min = NULL,
date_max = NULL,
yrwk_min = NULL,
yrwk_max = NULL,
time_total = FALSE,
location_code = NULL,
granularity_geo = "all",
location_reference = fhidata::norway_locations_names(),
...
) {
  if(!is.null(location_code) | !is.list(granularity_geo)){
    return(
      make_skeleton_single(
        date_min = date_min,
        date_max = date_max,
        yrwk_min = yrwk_min,
        yrwk_max = yrwk_max,
        time_total = time_total,
        location_code = location_code,
        granularity_geo = granularity_geo,
        location_reference = location_reference,
        ...
      )
    )
  } else {
    retval <- vector("list", length=length(granularity_geo))
    for(i in seq_along(granularity_geo)){
      retval[[i]] <- make_skeleton_single(
        date_min = date_min,
        date_max = date_max,
        yrwk_min = yrwk_min,
        yrwk_max = yrwk_max,
        time_total = time_total,
        location_code = location_code,
        granularity_geo = granularity_geo[[i]],
        location_reference = location_reference,
        ...
      )
    }
    names(retval) <- unlist(lapply(granularity_geo, function(x) paste0(x, collapse="+")))
    if(!is.null(names(granularity_geo))){
      index <- which(names(granularity_geo) != "")
      names(retval)[index] <- names(granularity_geo)[index]
    }

    return(retval)
  }
}

make_skeleton_single <- function(
  date_min = NULL,
  date_max = NULL,
  yrwk_min = NULL,
  yrwk_max = NULL,
  time_total = FALSE,
  location_code = NULL,
  granularity_geo = "all",
  location_reference = fhidata::norway_locations_names(),
  ...
  ) {
  retval <- list()
  if (!is.null(date_min) & !is.null(date_max)) {
    retval[[length(retval)+1]] <- make_skeleton_date(
      date_min = date_min,
      date_max = date_max,
      location_code = location_code,
      granularity_geo = granularity_geo,
      location_reference = location_reference,
      ...
    )
  }
  if (!is.null(yrwk_min) & !is.null(yrwk_max)) {
    retval[[length(retval)+1]] <- make_skeleton_week(
      yrwk_min = yrwk_min,
      yrwk_max = yrwk_max,
      location_code = location_code,
      granularity_geo = granularity_geo,
      location_reference = location_reference,
      ...
    )
  }
  if (time_total == TRUE) {
    retval[[length(retval)+1]] <- make_skeleton_total(
      location_code = location_code,
      granularity_geo = granularity_geo,
      location_reference = location_reference,
      ...
    )
  }

  if(length(retval)==0){
    stop("must provide one of the following: 1) date pair, 2) yrwk pair, 3) time_total=T")
  } else {
    retval <- rbindlist(retval)
  }
  return(retval)
}

make_skeleton_date <- function(
  date_min = NULL,
  date_max = NULL,
  location_code = NULL,
  granularity_geo = "all",
  location_reference = fhidata::norway_locations_names(),
  ...) {
  dates <- seq.Date(
    from = as.Date(date_min),
    to = as.Date(date_max),
    by = 1
  )

  locs <- NULL
  if (!is.null(location_code)) {
    locs <- location_code
  } else if ("all" %in% granularity_geo) {
    locs <- location_reference$location_code
  } else {
    x_gran <- granularity_geo
    locs <- location_reference[granularity_geo %in% x_gran]$location_code
  }
  retval <- expand.grid(
    ...,
    date = dates,
    location_code = locs,
    stringsAsFactors = FALSE
  )
  setDT(retval)
  retval[, granularity_time := "day"]
  retval[, granularity_geo := fhidata::get_granularity_geo(location_code)]
  setcolorder(retval, c("granularity_time","date","granularity_geo","location_code"))
  setorderv(retval, names(retval))

  return(retval)
}

make_skeleton_week <- function(
  yrwk_min = NULL,
  yrwk_max = NULL,
  location_code = NULL,
  granularity_geo = "all",
  location_reference = fhidata::norway_locations_names(),
  ...) {
  if(yrwk_min==yrwk_max){
    yrwks <- yrwk_min
  } else {
    yrwk_min <- which(fhidata::world_dates_isoyearweek$yrwk==yrwk_min)
    yrwk_max <- which(fhidata::world_dates_isoyearweek$yrwk==yrwk_max)
    yrwks <- fhidata::world_dates_isoyearweek[yrwk_min:yrwk_max,yrwk]
  }

  locs <- NULL
  if (!is.null(location_code)) {
    locs <- location_code
  } else if ("all" %in% granularity_geo) {
    locs <- location_reference$location_code
  } else {
    x_gran <- granularity_geo
    locs <- location_reference[granularity_geo %in% x_gran]$location_code
  }
  retval <- expand.grid(
    ...,
    yrwk = yrwks,
    location_code = locs,
    stringsAsFactors = FALSE
  )
  setDT(retval)
  retval[, granularity_time := "week"]
  retval[, granularity_geo := fhidata::get_granularity_geo(location_code)]
  setcolorder(retval, c("granularity_time","yrwk", "granularity_geo","location_code"))
  setorderv(retval, names(retval))
  return(retval)
}

make_skeleton_total <- function(
  location_code = NULL,
  granularity_geo = "all",
  location_reference = fhidata::norway_locations_names(),
  ...) {

  retval <- make_skeleton_week(
    yrwk_min = "1900-01",
    yrwk_max = "1900-01",
    location_code = location_code,
    granularity_geo = granularity_geo,
    location_reference = location_reference,
    ...
  )
  retval[, granularity_time := "total"]
  return(retval)
}
