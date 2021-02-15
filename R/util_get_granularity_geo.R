#' get_granularity_geo
#' @param x Datatable
#' @export
get_granularity_geo <- function(x){
  retval <- stringr::str_extract(x, "^[a-z]+")
  retval[retval=="norge"] <- "nation"
  retval
}
