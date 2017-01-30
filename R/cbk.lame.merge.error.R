#' Merge columns of mean and error interleavely
#' @param meanlame A pmlame of mean values.
#' @param errorlame A pmlame of error values.
#' @return A pmlame with values of both mean and error.
#' @export
#' @examples
#' pmlame        <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame0       <- pmlame[,!grepl("_error",colnames(pmlame))]
#' pmlame1       <- pmlame0[,setdiff(colnames(pmlame0), c("sample_id","image_id"))]
#' pmlame1_mean  <- cbk.lame.colMeans(pmlame1)
#' pmlame1_error <- cbk.lame.colSds(pmlame1)
#' pmlame2       <- cbk.lame.merge.error(pmlame1_mean,pmlame1_error)
cbk.lame.merge.error <- function(meanlame,errorlame) {

  chemlist  <- colnames(meanlame)
  errorlist <- colnames(errorlame)
  stonelist <- rownames(meanlame)

  ## Add NA to stone without error
  errorlame0           <- data.frame(match(stonelist,rownames(errorlame)))
  rownames(errorlame0) <- stonelist
  errorlame0           <- cbk.lame.rep(errorlame0, length(errorlist))
  colnames(errorlame0) <- errorlist
  for(ii in 1:length(stonelist)) {
    if (!is.na(errorlame0[ii,1])) {
      errorlame0[ii,] <- errorlame[errorlame0[ii,1],]
    }
  }

  ## Add NA to element without error
  chem_shared <- match(chemlist,errorlist)
  if (all(is.na(chem_shared))) {
    chem_shared <- match(paste0(chemlist,"_error"),errorlist)
  }
  errorlame1           <- data.frame(t(chem_shared))
  errorlame1           <- cbk.lame.rep(errorlame1, length(stonelist))
  rownames(errorlame1) <- stonelist
  for(ii in 1:length(chemlist)) {
    index <- errorlame1[1,ii]
    if (!is.na(index)) {
      colnames(errorlame1)[ii] <- errorlist[index]
      errorlame1[,ii]          <- errorlame0[,index]
    } else{
      colnames(errorlame1)[ii] <- chemlist[ii]
    }
  }

  ## Add "_error" to colnames of errorlame
  errorlist <- colnames(errorlame1)
  for(ii in 1:length(errorlist)) {
    if (!grepl("_error",errorlist[ii])) {
      colnames(errorlame1)[ii] <- paste0(errorlist[ii],"_error")
    }
  }

  ## Real work
  pmStat1 <- cbind(meanlame,errorlame1)
  n       <- ncol(meanlame)
  index   <- rep(1:n, each = 2) + (0:1) * n
  pmStat2 <- pmStat1[index]
  return(pmStat2)
}
