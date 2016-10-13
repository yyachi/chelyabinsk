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
  outfile <- tempfile(pattern = paste(stone[1],"@",sep=""), fileext = ".pml")
  ## system(paste("casteml download",stone[ii],">",outfile))
  cmd     <- paste("casteml download",stone)
  cat(system(cmd, intern = TRUE),file=outfile,sep="\n")
  return(outfile)
}
