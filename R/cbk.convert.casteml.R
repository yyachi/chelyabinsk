#' @title Convert CASTEML file to CSV file
#'
#' @description Convert CASTEML file to CSV file.  This function
#'   returns path to the converted file.  The converted file is stored
#'   in a temporary directory.  Note with the same arguments, this
#'   function convert file only once per a R session.  This is
#'   low-level function and users are not encourage to call this but
#'   cbk.read.casteml().
#'
#' @param pmlfile File path to CASTEML file
#' @param category Category filter that is passed to `casteml convert'
#' @param force Flag to force convert again
#' @return File path to the converted file
#' @export
#' @seealso \code{casteml convert},
#'   \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile    <- cbk.path("20081202172326.hkitagawa.pml")
#' dflame.csv <- cbk.convert.casteml(pmlfile)
#'
#' pmlfile    <- cbk.download.casteml("20081202172326.hkitagawa")
#' dflame.csv <- cbk.convert.casteml(pmlfile,category="trace")
cbk.convert.casteml <- function(pmlfile,category=NULL,force=FALSE) {
  ## cat(file=stderr(),"cbk.convert.casteml: pmlfile is |",pmlfile,"|\n")
  cat(file=stderr(),"cbk.convert.casteml:25: pmlfile # =>",pmlfile,"\n")

  ## outfile <- tempfile(pattern = sprintf("%s_%s@",tools::file_path_sans_ext(basename(pmlfile)),category), fileext=".dflame")
  ## outfile <- tempfile(fileext=".dflame")
  if(is.null(category)){
    cmd     <- paste("convert -f dataframe",pmlfile)
  } else {
    cmd     <- paste("convert -f dataframe -c",category,pmlfile)
  }
  outfile <- file.path(tempdir(),paste0(digest::digest(cmd,algo='md5'),".dflame"))

  ## Convert file only when it does not exist
  if (force || !file.exists(outfile)) {
    ## system(cmd,">",outfile))
    ## cat(system(cmd,intern=TRUE),file=outfile,sep="\n") # intern=T accepts 8095 char max per a line
    system2("casteml",cmd,stdout=outfile)
  }
  return(outfile)
}
