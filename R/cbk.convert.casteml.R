#' @title Convert CASTEML file to certain file
#'
#' @description Convert CASTEML file to certain file.  This function
#'   returns path to the converted file.  The converted file is stored
#'   in a temporary directory.  Note with the same arguments, this
#'   function convert file only once per a R session.  This is
#'   low-level function and users are not encourage to call this but
#'   cbk.read.casteml().
#'
#' @param pmlfile File path to CASTEML file.
#' @param category Category filter that is passed to `casteml convert'.
#' @param force Flag to force convert again.
#' @param verbose Output debug info (default: TRUE).
#' @param format Output format (default: dataframe).
#' @return Path of the converted file.
#' @export
#' @seealso \code{casteml convert} and
#'   \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile    <- cbk.path("20081202172326.hkitagawa.pml")
#' dflame.csv <- cbk.convert.casteml(pmlfile)
#'
#' pmlfile    <- cbk.download.casteml("20081202172326.hkitagawa")
#' texfile    <- cbk.convert.casteml(pmlfile,format="tex")
cbk.convert.casteml <- function(pmlfile,category=NULL,force=FALSE,verbose=TRUE,format="dataframe") {

  ##* Console
  if (verbose) {
    cat(file=stderr(),"cbk.convert.casteml:29: pmlfile # =>",pmlfile,"\n")
  }

  ##* Set extension list
  extlist        <- c(".dflame",".tex")
  names(extlist) <- c("dataframe","tex")
  ext            <- extlist[format]

  ##* Prepare output file
  if(is.null(category)){
    cmd     <- paste("convert -f",format,pmlfile)
  } else {
    cmd     <- paste("convert -f",format,"-c",category,pmlfile)
  }
  outfile <- file.path(tempdir(),paste0(digest::digest(cmd,algo='md5'),ext))

  ##* Convert file (only when it does not exist)
  if (force || !file.exists(outfile)) {
    ## system(cmd,">",outfile))
    ## cat(system(cmd,intern=TRUE),file=outfile,sep="\n") # intern=T accepts 8095 char max per a line
    if (verbose) {
      cat(file=stderr(),"cbk.convert.casteml:40: casteml # =>",cmd,"\n")
    }
    system2("casteml",cmd,stdout=outfile)
  }

  return(outfile)
}
