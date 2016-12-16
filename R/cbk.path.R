#' Return absolute path of files stored in local library
#' @param filename A filename
#' @return An absolute path of files stored in local library
#' @export
#' @examples
#' dflame.csv <- cbk.path("periodic-table1.dflame")
#' dflame.csv <- cbk.path("ref1.dflame")
#' pmlfile    <- cbk.path("20081202172326.hkitagawa.pml")
#' dflame.csv <- cbk.path("20081202172326.hkitagawa_trace.dataframe")
cbk.path <- function(filename){
  return(system.file("extdata", filename, package = "chelyabinsk"))
}
