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
  ChemList <- colnames(pmlame1)
  REEList  <- c("La","Ce","Pr","Nd","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu")
  OxyList  <- c("d18O","d17O")
  LiList   <- "d7Li"
  PbList   <- c("Pb206zPb204","Pb207zPb204","Pb208zPb204")

  if (any(REEList %in% ChemList)) {
    category <- "trace"
  } else if (any(OxyList %in% ChemList)) {
    category <- "oxygen"
  } else if (any(LiList %in% ChemList)) {
    category <- "lithium"
  } else if (any(PbList %in% ChemList)) {
    category <- "lead"
  } else {
    category <- "trace"
  }
  return(category)
}
