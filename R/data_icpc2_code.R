#' ICPC-2: The International Classification of Primary Care (Norwegian, English)
#'
#' Norwegian data is from 2020, English data is from 2018. (Link to be included)
#' @format
#' \describe{
#' \item{Code}{ICPC2 code}
#' \item{title_nb}{Description of the code in Norwegian.}
#' \item{title_en}{Description of the code in English.}
#' }
#' @source \url{https://ehelse.no/kodeverk/icpc-2.den-internasjonale-klassifikasjonen-for-primaerhelsetjenesten}
"icpc2_code"




gen_data_icpc2_code <- function(description_only = T) {

  # load norwegian data
  d_nb <- readxl::read_excel(system.file("rawdata","icpc2_code", "icpc2_code_nb_2020.xlsx", package = "fhidata"))
  colnames(d_nb) <- c('Code', 'short_title_nb', 'title')

  # load english data
  d_en <- readxl::read_excel(system.file("rawdata", "icpc2_code", "icpc2_code_en_2018.xlsx", package = "fhidata"))

  # join these two
  d_wide <- dplyr::left_join(d_nb, d_en, by = 'Code')

  if(description_only == F){
    d <- d_wide
  }
  d <- dplyr::select(d_wide, c('Code', 'short_title_nb', 'shortTitle'))

  # rename
  colnames(d) <- c('code', 'title_nb', 'title_en')


  setDT(d)
  return(d)
}

