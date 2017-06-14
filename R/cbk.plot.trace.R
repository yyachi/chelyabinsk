#' @title Read CASTEML file and create trace diagram
#'
#' @description Read CASTEML dataframe and create trace diagram.  This
#'   function does not save the created diagram.  You should prepare a
#'   canvas in advance.
#'
#' @param pmlfile_or_stone A CASTEML file that exits locally or
#'   stone-ID (or pmlame).
#' @param tableunit Unit to toss to \code{\link{cbk.read.casteml}}.
#' @param opts List of further options for plot.  See
#'   \code{\link{cbk.plot}}.
#' @return A pmlame used to plot the diagram.
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' pmlame  <- cbk.read.casteml(pmlfile,tableunit="ug/g",category=NULL)
#' cbk.plot.trace(pmlame,tableunit="ug/g")
cbk.plot.trace <- function(pmlfile_or_stone,opts=NULL,tableunit="none",...) {
  ## ----------------
  ##* PAGE SETUP
  ## ----------------
  par(mar=c(2.2,2.2,0.1,0.1),mfrow=c(2,1)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)

  ## ----------------
  ##* PLOTS
  ## ----------------
  pmlame  <- cbk.plot.spider(pmlfile_or_stone,opts,tableunit,...)
  pmlame1 <- cbk.plot.ree(pmlfile_or_stone,opts,tableunit,...)

  ## ----------------
  ##* CLOSING REMARK
  ## ----------------
  return(pmlame)
}
