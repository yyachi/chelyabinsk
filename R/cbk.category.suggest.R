#' Suggest categories to plot
#' @param pmlfile_or_stone A CASTEML file that exits locally or
#'   stone-ID (or pmlame) to survey categories.  Default is NULL and
#'   return all categories regardless their plotability.
#' @return Categories in preferred order
#' @export
#' @examples
#' pmlfile  <- cbk.path("20081202172326.hkitagawa.pml")
#' category <- cbk.category.suggest(pmlfile)
#' category <- cbk.category.suggest()
cbk.category.suggest <- function(pmlfile_or_stone=NULL) {
  if(is.null(pmlfile_or_stone)){
    category <- list(
      "default",
      "trace",
      "spider",
      "REE",
      "lithium",
      "oxygen",
      "lead")
  } else {
    pmlame0  <- cbk.read.casteml(pmlfile_or_stone)
    pmlame   <- cbk.filter.drop.dharma(pmlame0,column=TRUE)
    ChemList <- colnames(pmlame)
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
    category <- list()
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
  }
  return(category)
}
