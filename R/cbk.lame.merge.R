#' Merge to two pmlames
#' @param pmlame1 A pmlame with row of stone and column of chem [g/g].
#' @param pmlame2 Another pmlame.
#' @param verbose Output debug info (default: FALSE).
#' @return A combined pmlame with row of stone and column of chem
#' @seealso \code{cbk.lame.merge.error} and \code{merge}
#' @export
#' @examples
cbk.lame.merge <- function(pmlame1,pmlame2,verbose=FALSE) {

  if(verbose){
    cat(file=stderr(),"cbk.lame.merge:11: pmlame1 <- ")
    cbk.lame.stringfy(pmlame1)
    cat(file=stderr(),"cbk.lame.merge:13: pmlame2 <- ")
    cbk.lame.stringfy(pmlame2)
  }

  stone1 <- rownames(pmlame1)
  chem1  <- colnames(pmlame1)
  stone2 <- rownames(pmlame2)
  chem2  <- colnames(pmlame2)

  stone  <- union(rownames(pmlame1),rownames(pmlame2))
  chem   <- union(colnames(pmlame1),colnames(pmlame2))

  pmlame           <- as.data.frame(matrix(data=NA,nrow=length(stone),ncol=length(chem)))
  rownames(pmlame) <- stone
  colnames(pmlame) <- chem

  pmlame[stone1,chem1] <- pmlame1[stone1,chem1]
  pmlame[stone2,chem2] <- pmlame2[stone2,chem2]

  return(pmlame)
}
