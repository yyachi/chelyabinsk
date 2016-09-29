#' Convert filename of local library to absolute path
#' @param name A filename
#' @return A path to lib directory
#' @examples
#' castbl.path("periodic-table.csv")
#' @export
castbl.path <- function(name){
    return(system.file("data", name, package = "chelyabinsk"))
}
