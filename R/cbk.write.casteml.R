#' Write a pmlame to CASTEML file
#'
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param outfile File path to casteml file
#' @param verbose Output debug info (default: TRUE).
#' @return File path to a casteml file
#' @seealso \code{\link{cbk.read.casteml}}
#' @export
#' @examples
#' pmlame0 <- data.frame(row.names=c('ref_cpx_klb1@1','ref_cpx_klb1@2','trc_meso_allende@10'),SiO2=c(520000,520000,600000))
#' pmlfile <- cbk.write.casteml(pmlame0,'deleteme.pml')
cbk.write.casteml <- function(pmlame,outfile=NULL,verbose=TRUE){

  ##* Round number
  ## if(!is.null(digit)){
  ##   pmlame <- signif(pmlame,digit)
  ## }

  ##* Transform pmlame to have the first column labeled as `session'
  pmlame1 <- data.frame(session=rownames(pmlame),pmlame)
  ## rownames(pmlame1) <- NULL

  ##* Export pmlame to csvfile
  csvfile <- file.path(tempdir(),paste0(digest::digest(pmlame1,algo='md5'),".csv"))
  write.csv(pmlame1,csvfile,row.names=F)

  ##* Determine filename of pmlfile
  if(is.null(outfile)){
    outfile <- file.path(tempdir(),paste0(digest::digest(pmlame1,algo='md5'),".pml"))
  }

  ##* Convert csvfile to pmlfile
  cmd <- paste("convert -f pml",csvfile)
  if (verbose) {
    cat(file=stderr(),"cbk.write.casteml:30: casteml # =>",cmd,"\n")
  }
  system2("casteml",cmd,stdout=outfile)

  return(outfile)
}
