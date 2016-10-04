#' Return properties of elements from a file `periotic-table.csv'
#' @param property A name of PROPERTY such for atomicnumber,
#'   volatility, and compatibility
#' @return A numeric vector of element property with label
#' @export
#' @importFrom utils read.csv
#' @examples
#' cbk.periodic("atomicnumber")
#' cbk.periodic("volatility")
#' cbk.periodic("compatibility")
cbk.periodic <- function(property){
  ### EXAMPLES
  ### cbk.periodic("atomicnumber")
  ###  H Li Be  B  C  N  F Na Mg Al Si  P  S Cl  K Ca Ti Cr Mn Fe Ni Rb Sr  Y Zr Nb
  ###  1  3  4  5  6  7  9 11 12 13 14 15 16 17 19 20 22 24 25 26 28 37 38 39 40 41
  ### In Cs Ba La Ce Pr Nd Sm Eu Gd Tb Dy Ho Er Tm Yb Lu Hf Ta Tl Pb Bi Th  U
  ### 49 55 56 57 58 59 60 62 63 64 65 66 67 68 69 70 71 72 73 81 82 83 90 92

  foo        <- read.csv(cbk.path("periodic-table.csv"),row.names=1)
  bar        <- foo[,property]
  names(bar) <- rownames(foo)
  return(sort(bar))
}
