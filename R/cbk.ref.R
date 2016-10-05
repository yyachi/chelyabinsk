#' Return element abundances from file `ref.csv'
#' @param analysis A name of reference that is either "Boynton.1989", "McDonough.1995", "Wasson.1988"
#' @param property A numeric vector of element property with label
#'     that serves as sort key (default=NULL).  You can feed output from `cbk.periodic'
#' @return A numeric vector of element abundances with label
#' @examples
#' cbk.ref("Boynton.1989")
#' cbk.ref("Wasson.1988",cbk.periodic('atomicnumber'))
#' cbk.ref("McDonough.1995",cbk.periodic('volatility'))
#' @export
cbk.ref <- function(analysis,property=NULL){
  ## foo        <- c(.31,.808,.122,.6,.195,.0735,.2590,.0474,.322,.0718,.21,0.0324,.209,.0322)
  ## names(foo) <- c("La","Ce","Pr","Nd","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu")

  ### read as vector
  ## cbktbl     <- read.table(cbk.path("ref.csv"),sep=",",header=T,row.names=1)
  cbktbl        <- cbk.read.dataframe(cbk.path("ref1.dataframe"),"ppm")
  ## foo        <- as.numeric(cbktbl)
  foo           <- as.numeric(cbktbl[analysis,]) # conversion to numeric vector
  names(foo)    <- names(cbktbl)

  ### filter
  bar           <- foo[is.finite(foo) & !is.nan(foo) & !is.na(foo)]

  ### sort when sort-key is provided
  if(!is.null(property)){
    bar         <- bar[names(property)]
  }

  return(bar)
}
