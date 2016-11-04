#' @title Read csvfile created by casteml convert with column `unit'
#'
#' @description Read csvfile with column `unit' created by
#'   \code{casteml convert --format dataframe} .  This is low-level
#'   function and users are not encourage to call this but
#'   cbk.read.casteml().
#'
#' @details This internally calls
#'   \code{read.csv(csvfile,row.names=1,header=T,stringsAsFactors=F)},
#'   take out column of `unit' and normalized by the `unit' column.
#' @param csvfile A csvfile with columns of stone and rows of element
#'   abundances, with 2nd column `unit'
#' @param tableunit Output unit that will be resolved by
#'   cbk.convector() (default="none")
#' @return A dataframe with unit organized
#' @seealso \code{\link{cbk.download.casteml}}, \code{casteml
#'   convert}, \url{https://github.com/misasa/casteml},
#'   \code{\link{cbk.convector}},
#' @export
#' @examples
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' cbkfile <- cbk.convert.casteml(pmlfile,category="trace")
#' tbl0    <- cbk.read.dataframe(cbkfile,"ppm")
#'
#' tbl0    <- cbk.read.dataframe(cbk.path("20081202172326.hkitagawa_trace.dataframe"),"ppm")
#' tbl0    <- cbk.read.dataframe(cbk.path("ref1.dataframe"),"ppm")
#' tbl0    <- cbk.read.dataframe(cbk.path("periodic-table1.dataframe"))
cbk.read.dataframe <- function(csvfile,tableunit="none"){
  ## EXAMPLES
  ## $ casteml download -R 20130528105235-594267 > 20130528105235-594267.pml
  ## $ casteml convert -f dataframe -c trace 20130528105235-594267.pml > 20081202172326.hkitagawa_trace.dataframe
  ## R> cbktbl0 <- cbk.read.dataframe("20130528105235-594267.dataframe","ppm")
  tblin <- read.csv(csvfile,row.names=1,header=T,stringsAsFactors=F)
  if ('unit' %in% colnames(tblin)) {
    factor <- cbk.convector(tblin[,'unit'])
    names(factor) <- rownames(tblin)
    factor[is.na(factor)] <- 1
    cbktbl0 <- as.data.frame(t(tblin[colnames(tblin) != 'unit'] / factor)) * cbk.convector(tableunit)
  } else {
    cbktbl0 <- as.data.frame(t(tblin))
  }
  return(cbktbl0)
}
