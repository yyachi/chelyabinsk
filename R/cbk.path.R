#' Return absolute path of files stored in local library
#' @param filename A filename
#' @return An absolute path of files stored in local library
#' @export
#' @examples
#' cbkfile <- cbk.path("periodic-table1.dataframe")
#' cbkfile <- cbk.path("ref1.dataframe")
#' pmlfile <- cbk.path("20081202172326.kitagawa.pml")
#' cbkfile <- cbk.path("20081202172326.kitagawa_trace.dataframe")
cbk.path <- function(filename){
  return(system.file("extdata", filename, package = "chelyabinsk"))
}
