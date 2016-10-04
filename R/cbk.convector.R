#' Return conversion cofficient for unit
#' @param unit Unit string, that is one of
#'   none, g/g, wt\%, cg/g, \%, permil, mg/g, ppm, ug/g, ppb, ng/g, pg/g
#' @return Conversion cofficient.  When "permil", this returns 1000
#'   instead of 0.001.  Very likely you want to DIVIDE table by the factor
#' @export
#' @examples
#' cbk.convector("none")
#' cbk.convector("permil")
#' cbk.convector("ppm")
cbk.convector <- function(unit) {
  convector        <- c(1,     1,    100,  100,   100,1000,    1000,  1000000,1000000,1000000000,1000000000,1000000000000)
  names(convector) <- c("none","g/g","wt%","cg/g","%","permil","mg/g","ppm",  "ug/g", "ppb",     "ng/g",    "pg/g")
  return(convector[unit]);
}
