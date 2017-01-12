#' @title Read csvfile created by casteml convert with column `unit'
#'
#' @description Read csvfile with column `unit' created by
#'   \code{casteml convert --format dataframe} .  This is low-level
#'   function and users are not encourage to call this but
#'   cbk.read.casteml().
#'
#' @details This internally calls
#'   \code{read.csv(dflame.csv,row.names=1,header=T,stringsAsFactors=F)},
#'   take out column of `unit' and normalized by the `unit' column.
#' @param dflame.csv A csvfile with columns of stone and rows of element
#'   abundances, with 2nd column `unit'
#' @param tableunit Output unit that will be resolved by
#'   cbk.convector() (default="none")
#' @return A dataframe with unit organized
#' @seealso \code{\link{cbk.download.casteml}}, \code{casteml
#'   convert}, \url{https://github.com/misasa/casteml}, and
#'   \code{\link{cbk.convector}}
#' @export
#' @examples
#' pmlfile    <- cbk.download.casteml("20081202172326.hkitagawa")
#' dflame.csv <- cbk.convert.casteml(pmlfile,category="trace")
#' pmlame     <- cbk.read.dflame(dflame.csv,"ppm")
#'
#' pmlame     <- cbk.read.dflame(cbk.path("20081202172326.hkitagawa_trace.dflame"),"ppm")
#' pmlame     <- cbk.read.dflame(cbk.path("ref1-dflame0.csv"),"ppm")
#' pmlame     <- cbk.read.dflame(cbk.path("periodic-dflame0.csv"))
cbk.read.dflame <- function(dflame.csv,tableunit="none"){

  cat(file=stderr(),"cbk.read.dflame:30: dflame.csv # =>",dflame.csv,"\n")

  ## EXAMPLES
  ## $ casteml download -R 20130528105235-594267 > 20130528105235-594267.pml
  ## $ casteml convert -f dataframe -c trace 20130528105235-594267.pml > 20081202172326.hkitagawa_trace.dflame
  ## R> pmlame <- cbk.read.dflame("20130528105235-594267.dflame","ppm")
  qmlame <- read.csv(dflame.csv,row.names=1,header=T,stringsAsFactors=F)
  if ('unit' %in% colnames(qmlame)) {
    qmlame1               <- qmlame[rownames(qmlame) != 'file_path',]
    qmlame2               <- qmlame[rownames(qmlame) == 'file_path',]
    ## factor <- cbk.convector(qmlame[,'unit'])
    ## names(factor) <- rownames(qmlame)
    factor                <- cbk.convector(qmlame1[,'unit'])
    names(factor)         <- rownames(qmlame1)
    factor[is.na(factor)] <- 1
    qmlame3               <- apply(qmlame1,2,function(x) as.numeric(x) / factor * cbk.convector(tableunit))
    ## pmlame <- as.data.frame(t(qmlame[colnames(qmlame) != 'unit'] / factor)) * cbk.convector(tableunit)

    pmlame0               <- as.data.frame(t(qmlame3))
    pmlame1               <- cbind(pmlame0,t(qmlame2))
    pmlame                <- pmlame1[rownames(pmlame1) != 'unit',]
  } else { # without unit column
    pmlame <- as.data.frame(t(qmlame))
  }
  return(pmlame)
}
