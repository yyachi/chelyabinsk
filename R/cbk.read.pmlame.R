#' @title Get and return a pmlame
#'
#' @description Get and return a pmlame.  This function can use two
#'   pathways to get a pmlame.  The one downloads a json data from
#'   medusa, while the other downloads a CASTEML file using
#'   rubygem. Finally, they are converted to pmlames with same
#'   structure.
#' 
#' @param pmlfile_or_stone A CASTEML file that exists locally or
#'   stone-ID.
#' @param use_gem Download a CASTEML file into a temporary directory
#'   (default: FALSE).
#' @param opts List of further options for plot.
#' @param verbose Output debug info (default: TRUE).
#' @return A dataframe with unit organized.
#' @seealso \code{\link{cbk.convert.casteml}},
#'   \code{\link{cbk.read.dflame}}, and
#'   \url{https://github.com/misasa/casteml}
#' @export
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' pmlame  <- cbk.read.pmlame(pmlfile,tableunit="ppm",category="trace")
#' stone   <- "20081202172326.hkitagawa"
#' pmlame  <- cbk.read.pmlame(stone,tableunit="ppm")
cbk.read.pmlame <- function(pmlfile_or_stone,opts=NULL,use_gem=FALSE,verbose=TRUE,...){
  opts_default <- list(Recursivep=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)
  
  if (verbose) {
    cat(file=stderr(),
        "cbk.read.pmlame:32: pmlfile_or_stone # =>",
        ifelse(is.data.frame(pmlfile_or_stone),"#<pmlame>",pmlfile_or_stone),"\n")
  }

  if (use_gem || is.data.frame(pmlfile_or_stone) || file.exists(pmlfile_or_stone)) {
    pmlame <- cbk.read.casteml(pmlfile_or_stone,opts,verbose=verbose,...)
  } else {
    pmlame <- cbk.download.pmlame(pmlfile_or_stone,opts,verbose=verbose,...)
  }

  return(pmlame)
}
