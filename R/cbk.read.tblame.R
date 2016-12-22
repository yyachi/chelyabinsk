#' @title Read csvfile created by casteml convert with column `unit'
#'
#' @description Read csvfile with row `unit', that is also referred as
#'   `tblame'.
#'
#' @details This internally calls
#'   \code{read.csv(tblame,row.names=1,header=T,stringsAsFactors=F)},
#'   take out row of `unit' and normalized by the `unit' row.
#' @param tblame A csvfile with columns of element abundances and rows
#'   of stones, with 2nd row `unit'
#' @param tableunit Output unit that will be resolved by
#'   cbk.convector() (default="none")
#' @return A dataframe with unit organized
#' @seealso \code{\link{cbk.read.dflame}},
#'   \url{https://github.com/misasa/casteml}, and
#'   \code{\link{cbk.convector}}
#' @export
cbk.read.tblame <- function(tblame,tableunit="none"){

  ## cat(file=stderr(),"cbk.read.tblame:20: tblame is |",tblame,"|\n")
  cat(file=stderr(),"cbk.read.tblame:21: tblame # =>",tblame,"\n")

  pmlame <- read.csv(tblame,row.names=1,header=T,stringsAsFactors=F)
  if ('unit' %in% rownames(pmlame)) {
    factor                <- cbk.convector(as.matrix(pmlame['unit',]))
    names(factor)         <- colnames(pmlame)
    factor[is.na(factor)] <- 1
    pmlame                <- as.data.frame(t(apply(pmlame[rownames(pmlame) != 'unit',],1,function(x) as.numeric(x) / factor ))* cbk.convector(tableunit))
  }
  
  return(pmlame)
}
