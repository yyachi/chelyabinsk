#' @title Read CASTEML file and create trace diagram
#'
#' @description Read CASTEML dataframe and create trace diagram.  This
#'   function does not save the created diagram.  You should prepare a
#'   canvas in advance.
#'
#' @param pmlame A dataframe of element abundances (or pmlfile or stone-ID)
#' @param tableunit Unit to toss to cbk.read.casteml()
#' @param property Property to align x-axis
#' @param reference Reference of element abundance
#' @return Dataframe used to plot the diagram
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' pmlame  <- cbk.read.casteml(pmlfile,tableunit="ug/g",category=NULL)
#' cbk.plot.trace(pmlame)
cbk.plot.trace <- function(pmlame,tableunit="ug/g",property="atomicnumber",reference="Wasson.1988") {
  ## ----------------
  ##* PAGE SETUP
  ## ----------------
  par(mar=c(4.5,4.5,0.5,0.5),mfrow=c(2,1)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)

  ## ----------------
  ##* PLOTS
  ## ----------------
  pmlame  <- cbk.plot.spider(pmlame)
  pmlame1 <- cbk.plot.ree(pmlame)

  ## ----------------
  ##* CLOSING REMARK
  ## ----------------
  return(pmlame)
}
