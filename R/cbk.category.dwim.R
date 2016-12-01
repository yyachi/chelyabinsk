#' Return representative category
#' @param pmlame A dataframe with rows of stone and columns of chem
#' @return A representative category in the dataframe
#' @export
#' @examples
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' pmlame  <- cbk.read.casteml(pmlfile,"ppm",category=NULL)
#' category <- cbk.category.dwim(pmlame)
cbk.category.dwim <- function(pmlame) {
  pmlame1  <- cbk.filter.drop.dharma(pmlame,column=TRUE)
  chemlist <- colnames(pmlame1)
  REElist  <- c("La","Ce","Pr","Nd","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu")
  Olist    <- c("d18O","d17O")
  List     <- c("d7Li","Li")
  Pblist   <- c("Pb206zPb204","Pb207zPb204","Pb208zPb204")

  if (any(REElist %in% chemlist)) {
    category <- "trace"
  } else if (any(Olist %in% chemlist)) {
    category <- "oxygen"
  } else if (any(List %in% chemlist)) {
    category <- "lithium"
  } else if (any(Pblist %in% chemlist)) {
    category <- "lead"
  } else {
    category <- "trace"
  }
  return(category)
}
