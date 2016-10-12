#' Download analysis record in temporary directory and return the file path
#' 
#' @param ID Unique indentification number of stones in Medusa
#' @return file path to CASTEML file that was downloaded in temporary directory
#' @export
cbk.casteml.download <- function(stone) {
  ## EXAMPLES
  ## stone <- c("20160627191317-464538","20160627191900-040404","20160627191919-895636")
  ## cbk.casteml.download(stone)
  ## cbk.casteml.download("20160627191317-464538")
  outfiles <- c()
  for (ii in 1:length(stone)) {
    pmlfile <- tempfile(pattern = paste(stone[ii],".",sep=""), fileext = ".pml")
    system(paste("casteml download",stone[ii],">",pmlfile))
    ## pmlbuf  <- system(paste("casteml download",stone[ii]), intern = TRUE)
    ## save(pmlbuf, pmlfile)
    outfiles <- c(outfiles,pmlfile)
  }
  return(outfiles)
}
