#' Drop chem from pmlame
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param chem List of chem to drop from pmlame
#' @param verbose Output debug info (default: FALSE).
#' @return A pmlame without the specified chem
#' @seealso \code{cbk.lame.drop.chem}
#' @export
#' @examples
#' pmlame <- data.frame(row.names=c("stone@1","stone@2","stone@3"), SiO2=c(0.525,0.420,0.332), fo=c(0.353,3.400,20.323), d7Li=c(1.0,3.3,-1.0))
#' cbk.lame.drop.chem(pmlame,"fo")
#' cbk.lame.drop.chem(pmlame,c("fo","d7Li"))
cbk.lame.drop.chem <- function(pmlame,chem,verbose=FALSE) {
  idx <- colnames(pmlame) %in% chem
  if (any(idx)) {
    pmlame <- pmlame[,-which(idx),drop=FALSE]
  }
  return(pmlame)
}
