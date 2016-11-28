#' @title Read CASTEML file and return a pmlame
#'
#' @description Read CASTEML file and return a pmlame.
#'
#' @details This function converts a CASTEML file to a csvfile by
#'   `cbk.convert.casteml()' and read it by `cbk.read.dataframe()'.
#' @param pmlfile_or_stone A CASTEML file that exits locally or
#'   stone-ID
#' @param tableunit Output unit that will be resolved by
#'   cbk.convector() (default="none")
#' @param category category to pass to `cbk.convert.casteml'
#' @param download flag to directory download from Medusa
#'   (default=FALSE)
#' @return A dataframe with unit organized
#' @seealso \code{\link{cbk.convert.casteml}},
#'   \code{\link{cbk.read.dataframe}},
#'   \url{https://github.com/misasa/casteml}
#' @export
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' pmlame  <- cbk.read.casteml(pmlfile,tableunit="ppm",category="trace")
#'
#' pmlame  <- cbk.read.casteml("20081202172326.hkitagawa",tableunit="ppm",category="trace")
cbk.read.casteml <- function(pmlfile_or_stone,tableunit="none",category=NULL){

  ## cat(file=stderr(),"cbk.read.casteml: pmlfile_or_stone is |",pmlfile_or_stone,"|\n")
  cat(file=stderr(),"cbk.read.casteml:27: pmlfile_or_stone # =>",pmlfile_or_stone,"\n")
  
  if (is.data.frame(pmlfile_or_stone)) { # pmlame fed
    pmlame  <- pmlfile_or_stone
  } else {
    if (file.exists(pmlfile_or_stone)) { # existing-pmlfile fed
      pmlfile <- pmlfile_or_stone
    } else {                             # stone fed
      stone   <- pmlfile_or_stone
      pmlfile <- cbk.download.casteml(c("-r", stone))
    }
    pmlcsv  <- cbk.convert.casteml(pmlfile,category=category)
    pmlame  <- cbk.read.dataframe(pmlcsv,tableunit)
  }

  return(pmlame)
}
