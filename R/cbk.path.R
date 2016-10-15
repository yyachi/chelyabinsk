#' Convert filename of local library to absolute path
#' @param name A filename
#' @return A path to lib directory
#' @export
#' @examples
#' cbkfile <- cbk.path("periodic-table1.dataframe")
#' cbkfile <- cbk.path("ref1.dataframe")
#' pmlfile <- cbk.path("20081202172326.kitagawa.pml")
#' cbkfile <- cbk.path("20081202172326.kitagawa_trace.dataframe")
cbk.path <- function(name){
  return(system.file("extdata", name, package = "chelyabinsk"))
}
