#' Dates of different days within isoweekyears
#'
#' @format
#' \describe{
#' \item{isoyear}{Isoyear.}
#' \item{isoyearweek}{Isoweek-isoyear.}
#' \item{mon}{Date of Monday.}
#' \item{tue}{Date of Tuesday.}
#' \item{wed}{Date of Wednesday.}
#' \item{thu}{Date of Thursday.}
#' \item{fri}{Date of Friday.}
#' \item{sat}{Date of Saturday.}
#' \item{sun}{Date of Sunday.}
#' }
"world_dates_isoyearweek"

# Creates the norway_locations data.table
gen_world_dates_isoyearweek <- function() {
  . <- NULL
  year <- NULL
  yrwk <- NULL
  day <- NULL
  mon <- NULL
  tue <- NULL
  wed <- NULL
  thu <- NULL
  fri <- NULL
  sat <- NULL
  sun <- NULL

  days <- data.table(day = seq.Date(as.Date("1990-01-01"), as.Date("2040-01-01"), by = "days"))
  days[, year := as.integer(format.Date(day, format = "%G"))]
  days[, yrwk := format.Date(day, format = "%G-%V")]
  days[, isoyear := as.integer(format.Date(day, format = "%G"))]
  days[, isoyearweek := format.Date(day, format = "%G-%V")]
  days <- days[, .(mon = as.Date(min(day))), by = .(isoyear, isoyearweek, year, yrwk)]
  days[, tue := mon + 1]
  days[, wed := mon + 2]
  days[, thu := mon + 3]
  days[, fri := mon + 4]
  days[, sat := mon + 5]
  days[, sun := mon + 6]

  return(days)
}
