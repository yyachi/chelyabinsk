#' Suggest a category to plot
#' @param pmlame A dataframe with rows of stone and columns of chem
#' @return A representative category in the dataframe
#' @export
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' pmlame  <- cbk.read.casteml(pmlfile,"ppm",category=NULL)
#' category <- cbk.category.suggest(pmlame)
cbk.category.suggest <- function(pmlame) {
  pmlame1  <- cbk.filter.drop.dharma(pmlame,column=TRUE)
  ChemList <- colnames(pmlame1)
  REEList  <- c("La","Ce","Pr","Nd","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu")
  OxyList  <- c("d18O","d17O")
  LiList   <- "d7Li"
  PbList   <- c("Pb206zPb204","Pb207zPb204","Pb208zPb204")

  ### Suggest a category
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

  ### Make list of plottable cateogry
  ## foo <- c("default")
  ## if (any(REEList %in% ChemList)) {
  ##   foo <- append(foo, c("trace", "ree", "spider"))
  ## }
  ## if (any(OxyList %in% ChemList)) {
  ##   foo <- append(foo, "oxygen")
  ## }
  ## if (any(LiList %in% ChemList)) {
  ##   foo <- append(foo, "lithium")
  ## }
  ## if (any(PbList %in% ChemList)) {
  ##   foo <- append(foo, "lead")
  ## }
  
  return(category)
}
