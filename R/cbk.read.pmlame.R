#' @title Return a pmlame that is stored in Medusa
#'
#' @description Return a pmlame that is stored in Medusa.  There can
#'   be two approach to get the pmlame.  (1) The first approach is to
#'   downloads a CASTEML file using external program `gem package --
#'   casteml'.  (2) The other approach is to download a json data
#'   directoly from Medusa.
#'
#'   The essential functions involved in this function as of May 29
#'   (2018) are shown below.
#' 
#'   cbk.read.pmlame <returns pmlame>
#'   --> cbk.read.casteml <returns pmlame>
#'       --> cbk.download.casteml <returns pmlfile>
#'   --> cbk.download.pmlame <returns pmlame>
#'       --> medusaRClient.read.pmlame <returns pmlame>
#' 
#' @param pmlfile_or_stone A CASTEML file that exists locally or
#'   stone-ID.
#' @param casteml Download a CASTEML file using external program `gem package --
#'   casteml' (default: FALSE).
#' @param opts List of further options for plot.
#' @param verbose Output debug info (default: TRUE).
#' @return A dataframe with unit organized.
#' @seealso \code{\link{cbk.convert.casteml}},
#'   \code{\link{cbk.read.dflame}}, and
#'   \url{https://github.com/misasa/casteml}
#' @export
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' message(sprintf("The pmlfile is located at |%s|.",pmlfile))
#' pmlame  <- cbk.read.pmlame(pmlfile,tableunit="ppm",category="trace")
#' stone   <- "20081202172326.hkitagawa"
#' pmlame  <- cbk.read.pmlame(stone,tableunit="ppm")
cbk.read.pmlame <- function(pmlfile_or_stone,opts=NULL,casteml=FALSE,verbose=TRUE,...){
  opts_default <- list(Recursivep=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)
  
  if (verbose) {
    cat(file=stderr(),
        "cbk.read.pmlame:32: pmlfile_or_stone # =>",
        ifelse(is.data.frame(pmlfile_or_stone),"#<pmlame>",pmlfile_or_stone),"\n")
  }

  if (isTRUE(casteml) || is.data.frame(pmlfile_or_stone) || file.exists(pmlfile_or_stone)) {
    pmlame <- cbk.read.casteml(pmlfile_or_stone,opts,verbose=verbose,...)
  } else {
    pmlame <- cbk.download.pmlame(pmlfile_or_stone,opts,verbose=verbose,...)
  }

  return(pmlame)
}
