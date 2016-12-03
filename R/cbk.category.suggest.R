#' Suggest categories to plot
#' @param pmlame A dataframe with rows of stone and columns of chem
#' @return Categories in preferred order
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

  ### Suggest single category
  ## if (any(REEList %in% ChemList)) {
  ##   category <- "trace"
  ## } else if (any(OxyList %in% ChemList)) {
  ##   category <- "oxygen"
  ## } else if (any(LiList %in% ChemList)) {
  ##   category <- "lithium"
  ## } else if (any(PbList %in% ChemList)) {
  ##   category <- "lead"
  ## } else {
  ##   category <- "trace"
  ## }

  ### Suggest categories in preferred order
  category <- c()
  if (any(REEList %in% ChemList)) {
    category <- append(category, c("trace", "REE", "spider"))
  }
  if (any(OxyList %in% ChemList)) {
    category <- append(category, "oxygen")
  }
  if (any(LiList %in% ChemList)) {
    category <- append(category, "lithium")
  }
  if (any(PbList %in% ChemList)) {
    category <- append(category, "lead")
  }
  
  return(category)
}
