#' @title Return element abundances of reference
#'
#' @description Return element abundances of reference.  The reference
#'   includes "Wasson.1988", "McDonough.1995", and "Boynton.1989".
#'
#' [1] Wasson, J. T., and Kallemeyn, G. W. (1988). Compositions of chondrites. Phil. Trans. Roy. Soc. London A, 325, 535-544.
#' [2] McDonough, W., and Sun, S. (1995). The composition of the earth. Chemical Geology, 120(3-4), 223-253.
#' [3] Janousek, V. et al. (2015). Geochemical Modelling of Igneous Processes--Principles And Recipes in R Language. Springer
#'
#' @param analysis A name of reference that is either "Wasson.1988",
#'   "McDonough.1995", and "Boynton.1989".
#' @param property A numeric vector of element property with label
#'   that serves as sort key (default=NULL).  You can feed output from
#'   `cbk.periodic'
#' @param tableunit Output unit, that will be resolved by
#'   `cbk.convector' (default="ppm")
#' @return A numeric vector of element abundances with label
#' @seealso \code{\link{cbk.periodic}}
#' cbk.ref("Boynton.1989","ppm")
#' cbk.ref("Wasson.1988","ppm",cbk.periodic('atomicnumber'))
#' cbk.ref("Wasson.1988","ppm",cbk.periodic('volatility'))
#' cbk.ref("McDonough.1995","ppm",cbk.periodic('volatility'))
#' @export
cbk.ref <- function(analysis,tableunit="ppm",property=NULL){
  ## foo        <- c(.31,.808,.122,.6,.195,.0735,.2590,.0474,.322,.0718,.21,0.0324,.209,.0322)
  ## names(foo) <- c("La","Ce","Pr","Nd","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu")

  ### read as vector
  ## cbktbl     <- read.table(cbk.path("ref.csv"),sep=",",header=T,row.names=1)
  cbktbl        <- cbk.read.dataframe(cbk.path("ref1.dataframe"),tableunit)
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
