#' Merge columns of mean and error interleavely
#' @param pmlame_mean A pmlame of mean values
#' @param pmlame_error A pmlame of error values
#' @return A pmlame with values of both mean and error
#' @export
#' @examples
#' pmlame        <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame0       <- pmlame[,colnames(pmlame) != c("sample_id","image_id")]
#' pmlame1_mean  <- cbk.lame.colMeans(pmlame0)
#' pmlame1_error <- cbk.lame.colSds(pmlame0)
#' pmlame1       <- cbk.lame.merge.error(pmlame1_mean,pmlame1_error)
cbk.lame.merge.error <- function(pmlame_mean,pmlame_error) {

  chemlist  <- colnames(pmlame_mean)
  errorlist <- colnames(pmlame_error)
  stonelist <- rownames(pmlame_mean)

  ## Add NA to stone without error
  pmlame0_error           <- data.frame(match(stonelist,rownames(pmlame_error)))
  rownames(pmlame0_error) <- stonelist
  pmlame0_error           <- cbk.lame.rep(pmlame0_error, length(errorlist))
  colnames(pmlame0_error) <- errorlist
  for(ii in 1:length(stonelist)) {
    if (!is.na(pmlame0_error[ii,1])) {
      pmlame0_error[ii,] <- pmlame_error[pmlame0_error[ii,1],]
    }
  }

  ## Add NA to element without error
  chem_shared <- match(chemlist,errorlist)
  if (all(is.na(chem_shared))) {
    chem_shared <- match(paste0(chemlist,"_error"),errorlist)
  }
  pmlame1_error           <- data.frame(t(chem_shared))
  pmlame1_error           <- cbk.lame.rep(pmlame1_error, length(stonelist))
  rownames(pmlame1_error) <- stonelist
  for(ii in 1:length(chemlist)) {
    index <- pmlame1_error[1,ii]
    if (!is.na(index)) {
      colnames(pmlame1_error)[ii] <- errorlist[index]
      pmlame1_error[,ii]          <- pmlame0_error[,index]
    } else{
      colnames(pmlame1_error)[ii] <- chemlist[ii]
    }
  }

  ## Add "_error" to colnames of pmlame_error
  errorlist <- colnames(pmlame1_error)
  for(ii in 1:length(errorlist)) {
    if (!grepl("_error",errorlist[ii])) {
      colnames(pmlame1_error)[ii] <- paste0(errorlist[ii],"_error")
    }
  }

  ## Real work
  pmStat1 <- cbind(pmlame_mean,pmlame1_error)
  n       <- ncol(pmlame_mean)
  index   <- rep(1:n, each = 2) + (0:1) * n
  pmStat2 <- pmStat1[index]
  return(pmStat2)
}
