
#' Childhood vaccination rates in Norway (2019 borders)
#'
#' We conveniently package vaccine coverage data taken from "Kommunehelsa statistikkbank".
#' This data was last updated on 2019-04-09.
#'
#' This dataset contains national/county/municipality level (5 year average) vaccination coverage rates
#' for 16 year olds for the childhood vaccination program (diphtheria, hpv, measles,
#' mumps, poliomyelitis, pertussis, rubella, tetanus).
#'
#' Municipalities are updated for the 2019 borders.
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
"norway_childhood_vax_b2019"
