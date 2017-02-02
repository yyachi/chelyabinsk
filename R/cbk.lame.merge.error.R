#' Merge errorlame to pmlame interleavely
#' @param meanlame A pmlame of mean values with colname of chem.
#' @param errorlame An errorlame of error values with colname of chem.
#' @param verbose Output debug info (default: FALSE).
#' @return A pmlame with values of both mean and error.
#' @export
#' @examples
#' pmlame        <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame0       <- pmlame[,!grepl("_error",colnames(pmlame))]
#' pmlame1       <- pmlame0[,setdiff(colnames(pmlame0), c("sample_id","image_id"))]
#' pmlame1_mean  <- cbk.lame.colMeans(pmlame1)
#' pmlame1_error <- cbk.lame.colSds(pmlame1)
#' pmlame2       <- cbk.lame.merge.error(pmlame1_mean,pmlame1_error)
cbk.lame.merge.error <- function(meanlame,errorlame,verbose=FALSE) {

  if(verbose){
    cat(file=stderr(),"cbk.lame.merge.error:17: meanlame <- ")
    cbk.lame.stringfy(meanlame)
    cat(file=stderr(),"cbk.lame.merge.error:17: errorlame <- ")
    cbk.lame.stringfy(errorlame)
  }

  chemlist          <- colnames(meanlame)
  stonelist         <- rownames(meanlame)
  chem_error_regexp <- "^([A-Za-z].*)_error"
  errorlist         <- gsub(chem_error_regexp,"\\1",colnames(errorlame)) # Li_error -> Li

  ## Add NA to stone without error
  errorlame0           <- data.frame(match(stonelist,rownames(errorlame)))
  rownames(errorlame0) <- stonelist
  errorlame0           <- cbk.lame.rep(errorlame0,length(errorlist),direction='col')
  colnames(errorlame0) <- errorlist
  for(ii in 1:length(stonelist)) {
    index <- errorlame0[ii,1]
    if (!is.na(index)) {
      errorlame0[ii,] <- errorlame[index,]
    }
  }

  ## Add NA to element without error
  chem_shared          <- match(chemlist,errorlist)
  errorlame1           <- data.frame(t(chem_shared))
  errorlame1           <- cbk.lame.rep(errorlame1, length(stonelist),direction='row')
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
  colnames(errorlame1) <- paste0(colnames(errorlame1),"_error")

  ## Real work
  pmStat1 <- cbind(meanlame,errorlame1)
  n       <- ncol(meanlame)
  index   <- rep(1:n, each = 2) + (0:1) * n
  pmStat2 <- pmStat1[index]
  return(pmStat2)
}
