#' Merge columns of mean and standard-error interleavely
#' @param pmlame A pmlame of mean values
#' @param pmlame_error A pmlame of error values
#' @return A pmlame with merged columns
#' @export
#' @examples
#' pmlame            <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame0           <- pmlame[,colnames(pmlame) != c("sample_id","image_id")]
#' pmlame1           <- cbk.lame.colMeans(pmlame0)
#' pmlame1_error     <- cbk.lame.colSds(pmlame0)
#' pmlame_with_error <- cbk.lame.merge.error(pmlame1,pmlame1_error)
cbk.lame.merge.error <- function(pmlame,pmlame_error) {

  # Add "_error" to colnames of pmlame_error
  chemlist <- colnames(pmlame_error)
  for(ii in 1:length(chemlist)) {
    if (!grepl("error",chemlist[ii])) {
      colnames(pmlame_error)[ii] <- paste(chemlist[ii],"error",sep="_")
    }
  }

  # Real work
  pmStat1 <- cbind(pmlame,pmlame_error)
  n       <- ncol(pmlame)
  index   <- rep(1:n, each = 2) + (0:1) * n
  pmStat2 <- pmStat1[index]
  return(pmStat2)
}
