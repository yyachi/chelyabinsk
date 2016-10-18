#' @title Convert CASTEML file to CSV file
#'
#' @description Convert CASTEML file to CSV file.  This function
#'   returns path to the converted file.  The converted file is stored
#'   in a temporary directory.  This is low-level function and users
#'   are not encourage to call this but cbk.read.casteml().
#' 
#' @param pmlfile File path to CASTEML file
#' @param category Category filter that is passed to `casteml convert'
#' @return File path to the converted file
#' @export
#' @seealso \code{casteml convert},
#'   \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' cbkfile <- cbk.convert.casteml(pmlfile)
#'
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' cbkfile <- cbk.convert.casteml(pmlfile,category="trace")
cbk.convert.casteml <- function(pmlfile,category=NULL) {
  ## outfile <- tempfile(pattern = sprintf("%s_%s@",tools::file_path_sans_ext(basename(pmlfile)),category), fileext=".dataframe")
  outfile <- tempfile(fileext=".dataframe")
  if(is.null(category)){
    cmd     <- paste("casteml convert -f dataframe",pmlfile)
  } else {
    cmd     <- paste("casteml convert -f dataframe -c",category,pmlfile)
  }
  cat(system(cmd, intern = TRUE),file=outfile,sep="\n")
  return(outfile)
}
