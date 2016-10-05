#' @title Read csvfile created by casteml convert with column `unit'
#'
#' @description Read csvfile created by \code{casteml convert --format dataframe} with
#'   column `unit'.
#'
#' @details This internally calls
#'   \code{read.csv(csvfile,row.names=1,header=T,stringsAsFactors=F)},
#'   take out column of `unit' and normalized by the `unit' column.
#' @param csvfile A csvfile with columns of stone and rows of element
#'   abundances, with 2nd column `unit'
#' @param unit Preferred unit that will be passed to `cbk.convector'
#' @return A cbk.dataframe with unit organized
#' @seealso \url{http://dream.misasa.okayama-u.ac.jp},
#'   \code{\link{cbk.convector}}
#' @export
#' @examples
#' cbk.read.dataframe(cbk.path("20130528105235-594267_trace.dataframe"),"ppm")
cbk.read.dataframe <- function(csvfile,unit){
  ### EXAMPLES
  ### $ casteml download -R 20130528105235-594267 > 20130528105235-594267.pml
  ### $ casteml convert -f dataframe -c trace 20130528105235-594267.pml > 20130528105235-594267_trace.dataframe
  ### R> cbktbl0 <- cbk.read.dataframe("20130528105235-594267.dataframe","ppm")
  tblin <- read.csv(csvfile,row.names=1,header=T,stringsAsFactors=F)
  if ('unit' %in% colnames(tblin)) {
    factor <- cbk.convector(tblin[,'unit'])
    names(factor) <- rownames(tblin)
    factor[is.na(factor)] <- 1
    cbktbl0 <- as.data.frame(t(tblin[colnames(tblin) != 'unit'] / factor)) * cbk.convector(unit)
  } else {
    cbktbl0 <- as.data.frame(t(tblin))
  }
  return(cbktbl0)
}
