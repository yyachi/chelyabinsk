#' @title Read CASTEML file and create trace diagram
#'
#' @description Read CASTEML dataframe and create trace diagram.  This
#'   function does not save the created diagram.  You should prepare a
#'   canvas in advance.
#'
#' @param pmlfile_or_stone A CASTEML file that exits locally or stone-ID (or pmlame)
#' @param opts List of further options for plot
#' @return Dataframe used to plot the diagram
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' pmlame  <- cbk.read.casteml(pmlfile,tableunit="ug/g",category=NULL)
#' cbk.plot.trace(pmlame)
cbk.plot.trace <- function(pmlfile_or_stone,opts=NULL) {
  ## ----------------
  ##* PAGE SETUP
  ## ----------------
  par(mar=c(4.5,4.5,0.5,0.5),mfrow=c(2,1)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)

  ## ----------------
  ##* PLOTS
  ## ----------------
  pmlame  <- cbk.plot.spider(pmlfile_or_stone,opts)
  pmlame1 <- cbk.plot.ree(pmlfile_or_stone,opts)

  ## ----------------
  ##* CLOSING REMARK
  ## ----------------
  return(pmlame)
}
