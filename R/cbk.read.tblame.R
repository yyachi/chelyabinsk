#' @title Read csvfile created by casteml convert with column `unit'
#'
#' @description Read csvfile with row `unit', that is also referred as
#'   `tblame'.
#'
#' @details This internally calls \code{\link{read.csv}}, take out row
#'   of `unit' and normalized by the `unit' row.
#' @param tblame A csvfile with columns of chem and rows of stone,
#'   with 2nd row `unit'
#' @param tableunit Output unit that will be resolved by
#'   \link{cbk.convector} (default: "none").
#' @param verbose Output debug info (default: TRUE).
#' @return A pmlame with unit organized.
#' @seealso \code{\link{cbk.read.dflame}},
#'   \url{https://github.com/misasa/casteml}, and
#'   \code{\link{cbk.convector}}.
#' @export
cbk.read.tblame <- function(tblame,tableunit="none",verbose=TRUE){

  ## cat(file=stderr(),"cbk.read.tblame:20: tblame is |",tblame,"|\n")
  if (verbose) {
    cat(file=stderr(),"cbk.read.tblame:23: tblame # =>",tblame,"\n")
  }

  ## pmlame <- read.csv(tblame,row.names=1,header=T,stringsAsFactors=F)
  pmlame <- read.csv(tblame,row.names=1,header=T,stringsAsFactors=F,check.names=F)

  if ('unit' %in% rownames(pmlame)) {
    colSTR <- intersect(colnames(pmlame),c("image_path","sample_id","image_id","remark"))
    if (length(rowSTR)==0) {
      factor                <- cbk.convector(as.matrix(pmlame['unit',]))
      names(factor)         <- colnames(pmlame)
      factor[is.na(factor)] <- 1
      pmlame                <- as.data.frame(t(apply(pmlame[rownames(pmlame) != 'unit',],1,function(x) as.numeric(x) / factor ))* cbk.convector(tableunit))
    } else {
      pmlameSTR             <- pmlame[,colSTR,drop=FALSE]
      pmlameNUM             <- pmlame[,colnames(pmlame) != colSTR,drop=FALSE]
      factor                <- cbk.convector(as.matrix(pmlameNUM['unit',]))
      names(factor)         <- colnames(pmlameNUM)
      factor[is.na(factor)] <- 1
      pmlameNUM             <- as.data.frame(t(apply(pmlameNUM,1,function(x) as.numeric(x) / factor )* cbk.convector(tableunit)))
      pmlame0               <- cbind(pmlameNUM,pmlameSTR)
      pmlame                <- pmlame0[rownames(pmlame0) != 'unit',]
    }
  }
  return(pmlame)
}
