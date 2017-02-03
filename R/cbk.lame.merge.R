#' Merge to two pmlames
#' @param ... Multiple pmlames with row of stone and column of chem [g/g].
#' @param verbose Output debug info (default: FALSE).
#' @return A merged pmlame with row of stone and column of chem.
#' @seealso \code{cbk.lame.merge.error} and \code{merge}
#' @export
#' @examples
#' pmlame1 <- data.frame(row.names=c("stone@1","stone@2"), SiO2=c(0.525,0.420), fo=c(0.353,3.400), d7Li=c(1.0,3.3))
#' pmlame2 <- structure(list(fo = c(0.98,0.74), d18O = c(4.8,2.5)), row.names = c("ol_chd1@5","cpx_chd1@6"), .Names = c("fo","d18O"), class = "data.frame")
#' cbk.lame.merge(pmlame1,pmlame2)
cbk.lame.merge <- function(...,verbose=FALSE) {
  pmlames <- list(...)

  if(verbose){
    for(ii in 1:length(pmlames)) {
      cat(file=stderr(),sprintf("cbk.lame.merge:16: pmlames[[%d]] <- ",ii))
      cbk.lame.stringfy(pmlames[[ii]])
    }
  }

  stone  <- c();
  chem   <- c();

  for(lame in pmlames) {
    stone_i <- rownames(lame)
    chem_i  <- colnames(lame)

    stone <- union(stone,stone_i)
    chem  <- union(chem,chem_i)
  }

  pmlame           <- as.data.frame(matrix(data=NA,nrow=length(stone),ncol=length(chem)))
  rownames(pmlame) <- stone
  colnames(pmlame) <- chem

  for(lame in pmlames) {
    stone_i <- rownames(lame)
    chem_i  <- colnames(lame)

    pmlame[stone_i,chem_i] <- lame[stone_i,chem_i]
  }

  return(pmlame)
}
