#' Return absolute path of files stored in local library
#' @param filename A filename
#' @return An absolute path of files stored in local library
#' @export
#' @examples
#' dflame.csv <- cbk.path("periodic-dflame1.csv")
#' dflame.csv <- cbk.path("ref1-dflame1.csv")
#' pmlfile    <- cbk.path("20081202172326.hkitagawa.pml")
#' dflame.csv <- cbk.path("20081202172326.hkitagawa_trace.dflame")
cbk.path <- function(filename){
  return(system.file("extdata", filename, package = "chelyabinsk"))
}
