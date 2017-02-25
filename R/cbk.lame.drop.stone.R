#' Drop stone from pmlame
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param stone List of stone to drop from pmlame
#' @return A pmlame without the specified stone
#' @seealso \code{cbk.lame.drop.stone}
#' @export
#' @examples
#' pmlame <- data.frame(row.names=c("stone@1","stone@2","stone@3"), SiO2=c(0.525,0.420,0.332), fo=c(0.353,3.400,20.323), d7Li=c(1.0,3.3,-1.0))
#' cbk.lame.drop.stone(pmlame,"stone@1")
#' cbk.lame.drop.stone(pmlame,c("stone@1","stone@2"))
cbk.lame.drop.stone <- function(pmlame,stone,verbose=FALSE) {
  idx <- rownames(pmlame) %in% stone
  if (any(idx)) {
    pmlame <- pmlame[-which(idx),,drop=FALSE]
  }
  return(pmlame)
}
