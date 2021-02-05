.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "fhidata",
    utils::packageDescription("fhidata")$Version,
    "https://folkehelseinstituttet.github.io/fhidata"
  ))
}
