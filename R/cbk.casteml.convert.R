#' Convert CASTEML files to temporary directory and return the file path
#' @param pmlfile file path to CASTEML file
#' @param category category to pass to `casteml convert'
#' @return file path to converted file in temporary directory
#' @export
cbk.casteml.convert <- function(pmlfile,category="trace") {
  ## EXAMPLES
  ## pmlfile <- cbk.casteml.download("20160627191317-464538")
  ## cbk.casteml.convert(pmlfile,category="trace")
 
  outfile <- tempfile(pattern = sprintf("%s_%s@",tools::file_path_sans_ext(basename(pmlfile)),category), fileext=".dataframe")
  ## system(paste("casteml convert -f dataframe -c ", category, pmlfile,">", outfile))
  cmd     <- paste("casteml convert -f dataframe -c ", category, pmlfile)
  cat(system(cmd, intern = TRUE),file=outfile,sep="\n")
  return(outfile)
}
