#' Convert filename of local library to absolute path
#' @param name A filename
#' @return A path to lib directory
#' @export
#' @examples
#' cbk.path("periodic-table.csv")
#' cbk.path("ref.csv")
#' cbk.path("ref1.dataframe")
cbk.path <- function(name){
  return(system.file("extdata", name, package = "chelyabinsk"))
}
