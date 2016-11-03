#' @title Read CASTEML file and create trace diagram
#'
#' @description Read CASTEML file and create trace diagram.  This
#'   function does not save the created diagram.  You should prepare a
#'   canvas in advance.
#'
#' @param pmlfile File path to CASTEML file
#' @param tableunit Unit to toss to cbk.read.casteml()
#' @param property Property to align x-axis
#' @param reference Reference of element abundance
#' @return Dataframe used to plot the diagram
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' cbk.plot.trace(pmlfile)
cbk.plot.trace <- function(pmlfile,tableunit="ug/g",property="atomicnumber",reference="Wasson.1988") {
  ### ----------------
  ###* PAGE SETUP
  ### ----------------
  par(mar=c(4.5,4.5,0.5,0.5),mfrow=c(2,1)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)

  ### ----------------
  ###* PLOTS
  ### ----------------
  tbl0 <- cbk.plot.spider(pmlfile)
  tbl1 <- cbk.plot.ree(pmlfile)
  
  ### ----------------
  ###* CLOSING REMARK
  ### ----------------
  return(tbl0)
}
