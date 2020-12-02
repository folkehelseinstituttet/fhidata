#' ICPC-2: The International Classification of Primary Care (Norwegian)
#'
#' @format
#' \describe{
#' \item{Code}{ICPC2 code (2020)}
#' \item{shortTitle_nb}{Description of the code in Norwegian.}
#' }
#' @source \url{https://ehelse.no/kodeverk/icpc-2.den-internasjonale-klassifikasjonen-for-primaerhelsetjenesten}
"icpc2_code_nb_2020"


#' ICPC-2: The International Classification of Primary Care
#'
#' @format
#' \describe{
#' \item{Code}{ICPC2 code (2018)}
#' \item{shortTitle}{Description of the code.}
#' }
#' @source \url{https://ehelse.no/kodeverk/icpc-2e--english-version}
"icpc2_code_en_2018"



gen_data_icpc2_code_nb <- function(description_only = T) {

  d <- readxl::read_excel(system.file("rawdata","icpc2_code", "icpc2_code_nb_2020.xlsx", package = "fhidata"))
  colnames(d) <- c('Code', 'shortTitle_nb', 'title')

  if(description_only == F){
    d <- d
  }
  d <- dplyr::select(d, c('Code', 'shortTitle_nb'))

  setDT(d)
  return(d)
}




gen_data_icpc2_code_en <- function(description_only = T) {

  d <- readxl::read_excel(system.file("rawdata", "icpc2_code", "icpc2_code_en_2018.xlsx", package = "fhidata"))

  if(description_only == F){
    d <- d
  }
  d <- dplyr::select(d, c('Code', 'shortTitle'))

  setDT(d)
  return(d)
}





