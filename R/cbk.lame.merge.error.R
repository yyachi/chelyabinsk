#' Merge columns of mean and error interleavely
#' @param pmlame_mean A pmlame of mean values
#' @param pmlame_error A pmlame of error values
#' @return A pmlame with values of both mean and error
#' @export
#' @examples
#' pmlame            <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame0           <- pmlame[,colnames(pmlame) != c("sample_id","image_id")]
#' pmlame1_mean      <- cbk.lame.colMeans(pmlame0)
#' pmlame1_error     <- cbk.lame.colSds(pmlame0)
#' pmlame1           <- cbk.lame.merge.error(pmlame1_mean,pmlame1_error)
cbk.lame.merge.error <- function(pmlame_mean,pmlame_error) {

  ## chemlist     <- colnames(pmlame_mean)
  ## stonelist    <- rownames(pmlame_mean)
  ## pmlame_error <- pmlame_error[stonelist,chemlist]
  
  ## Add "_error" to colnames of pmlame_error
  chemlist <- colnames(pmlame_error)
  for(ii in 1:length(chemlist)) {
    if (!grepl("error",chemlist[ii])) {
      colnames(pmlame_error)[ii] <- paste0(chemlist[ii],"_error")
    }
  }

  ## Real work
  pmStat1 <- cbind(pmlame_mean,pmlame_error)
  n       <- ncol(pmlame_mean)
  index   <- rep(1:n, each = 2) + (0:1) * n
  pmStat2 <- pmStat1[index]
  return(pmStat2)
}
