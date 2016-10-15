#' @title Convert CASTEML file to CSV file
#'
#' @description Convert CASTEML file to CSV file.  The converted file
#'   is stored in a temporary directory.  This function returns path
#'   to the file.
#' 
#' @param pmlfile file path to CASTEML file
#' @param category category to pass to `casteml convert'
#' @return file path to converted file in temporary directory
#' @export
#' @seealso \code{casteml convert}, \url{https://github.com/misasa/casteml}, \code{\link{cbk.casteml.download}}
cbk.casteml.convert <- function(pmlfile,category="trace") {
  ## EXAMPLES
  ## pmlfile <- cbk.casteml.download("20160627191317-464538")
  ## cbk.casteml.convert(pmlfile,category="trace")

  ## outfile <- tempfile(pattern = sprintf("%s_%s@",tools::file_path_sans_ext(basename(pmlfile)),category), fileext=".dataframe")
  outfile <- tempfile(fileext=".dataframe")
  ## system(paste("casteml convert -f dataframe -c ", category, pmlfile,">", outfile))
  cmd     <- paste("casteml convert -f dataframe -c ", category, pmlfile)
  cat(system(cmd, intern = TRUE),file=outfile,sep="\n")
  return(outfile)
}
