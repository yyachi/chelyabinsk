#' Return conversion cofficient depending on unit
#' @param tableunit Unit string, that is one of
#'   c("none","g/g","wt%","cg/g","%","permil","mg/g","ppm","ug/g","ppb","ng/g","pg/g")
#' @return Conversion cofficient.  When `permil', this returns 1000 instead of 0.001
#' @export
#' @examples
#' cas.convector("permil")
cas.convector <- function(tableunit) {
  convector        <- c(1,     1,    100,  100,   100,1000,    1000,  1000000,1000000,1000000000,1000000000,1000000000000)
  names(convector) <- c("none","g/g","wt%","cg/g","%","permil","mg/g","ppm",  "ug/g", "ppb",     "ng/g",    "pg/g")
  return(convector[tableunit]);
}
