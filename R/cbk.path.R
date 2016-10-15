#' Convert filename of local library to absolute path
#' @param name A filename
#' @return A path to lib directory
#' @export
#' @examples
#' cbk.path("periodic-table1.dataframe")
#' cbk.path("ref1.dataframe")
#' cbk.path("20081202172326.kitagawa.pml")
#' cbk.path("20081202172326.kitagawa_trace.dataframe")
cbk.path <- function(name){
  return(system.file("extdata", name, package = "chelyabinsk"))
}
