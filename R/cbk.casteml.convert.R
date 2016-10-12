#' Convert CASTEML files to temporary directory and return the file path
#' @param pmlfile file path to CASTEML file
#' @param category category to pass to `casteml convert'
#' @return file path to converted file in temporary directory
cbk.casteml.convert <- function(pmlfile,category="trace") {
  ## EXAMPLES
  ## pmlfile <- cbk.casteml.download("20160627191317-464538")
  ## cbk.casteml.convert(pmlfile,category="trace")

  outfiles <- c()
  ## ext <- sprintf("_%s.dataframe",categoray)
  for(ii in 1:length(pmlfile)) { 
    outfile <- tempfile(pattern = paste(category,".",sep=""), fileext = ".dataframe")
    system(paste("casteml convert -f dataframe -c ", category, pmlfile[ii],">", outfile))
    outfiles <- c(outfiles,outfile)
  }
  return(outfiles)
}
