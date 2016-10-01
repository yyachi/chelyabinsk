#' Return element abundances from file `ref.csv'
#' @param analysis A name of ANALYSIS such like "Boynton 1989"
#' @param property A numeric vector of element property with label
#'     that serves as sort key (option)
#' @return A numeric vector of element abundances with label
#' @examples
#' > cas.ref("Boynton 1989")
#'     La     Ce     Pr     Nd     Sm     Eu     Gd     Tb     Dy     Ho     Er
#' 0.3100 0.8080 0.1220 0.6000 0.1950 0.0735 0.2590 0.0474 0.3220 0.0718 0.2100
#'     Tm     Yb     Lu
#' 0.0324 0.2090 0.0322
#' @export
cas.ref <- function(analysis,property=NULL){
  ## foo        <- c(.31,.808,.122,.6,.195,.0735,.2590,.0474,.322,.0718,.21,0.0324,.209,.0322)
  ## names(foo) <- c("La","Ce","Pr","Nd","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu")

  ### read as vector
  castbl        <- read.table(castbl.path("ref.csv"),sep=",",header=T,row.names=1)
  ## foo        <- as.numeric(castbl)
  foo           <- as.numeric(castbl[analysis,]) # conversion to numeric vector
  names(foo)    <- names(castbl)

  ### filter
  bar           <- foo[is.finite(foo) & !is.nan(foo) & !is.na(foo)]

  ### sort if sort-key is provided
  if(!is.null(property)){
    bar       <- bar[names(property)]
  }

  return(bar)
}
