#' @title Convert CASTEML file to CSV file
#'
#' @description Convert CASTEML file to CSV file.  This function
#'   returns path to the converted file.  The converted file is stored
#'   in a temporary directory.
#' 
#' @param pmlfile file path to CASTEML file
#' @param category category to pass to `casteml convert'
#' @return file path to converted file
#' @export
#' @seealso \code{casteml convert}, \url{https://github.com/misasa/casteml}, \code{\link{cbk.download.casteml}}
#' @examples
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' cbkfile <- cbk.convert.casteml(pmlfile,category="trace")
#'
#' cbkfile <- cbk.convert.casteml(cbk.path("20081202172326.hkitagawa.pml"),category="trace")
cbk.convert.casteml <- function(pmlfile,category=NULL) {
  ## outfile <- tempfile(pattern = sprintf("%s_%s@",tools::file_path_sans_ext(basename(pmlfile)),category), fileext=".dataframe")
  outfile <- tempfile(fileext=".dataframe")
  if(is.null(category)){
    cmd     <- paste("casteml convert -f dataframe -c", category, pmlfile)
  } else {
    cmd     <- paste("casteml convert -f dataframe", pmlfile)
  }
  cat(system(cmd, intern = TRUE),file=outfile,sep="\n")
  return(outfile)
}
