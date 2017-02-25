#' Drop chem from pmlame
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param chem List of chem to drop from pmlame
#' @return A pmlame without the specified chem
#' @seealso \code{cbk.lame.drop.chem}
#' @export
#' @examples
#' pmlame <- data.frame(row.names=c("stone@1","stone@2","stone@3"), SiO2=c(0.525,0.420,0.332), fo=c(0.353,3.400,20.323), d7Li=c(1.0,3.3,-1.0))
#' cbk.lame.drop.chem(pmlame,"fo")
#' cbk.lame.drop.chem(pmlame,c("fo","d7Li"))
cbk.lame.drop.chem <- function(pmlame,chem,verbose=FALSE) {
  pmlame1 <- pmlame[,-which(colnames(pmlame) %in% chem),drop=FALSE]
  return(pmlame1)
}
