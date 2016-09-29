#' Return properties of elements from a file "periotic-table.csv"
#' @param property A name of PROPERTY such for atomicnumber, volatility, and compatibility
#' @return A numeric vector of element property with label
#' @examples
#' cas.periodic("atomicnumber")['H']
#' @export
cas.periodic <- function(property){
    foo        <- read.csv(castbl.path("periodic-table.csv"),row.names=1)
    bar        <- foo[,property]
    names(bar) <- rownames(foo)
    return(sort(bar))
}
