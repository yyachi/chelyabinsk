#' @title Read CASTEML file and create lead diagram
#'
#' @description Read CASTEML file and create lead diagram.  This
#'   function does not save the created diagram.  You should prepare a
#'   canvas in advance.
#'
#' @param pmlfile File path to CASTEML file
#' @param tableunit Unit to toss to cbk.read.casteml()
#' @return Dataframe used to plot the diagram
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' cbk.plot.lead(pmlfile)
cbk.plot.lead <- function(pmlfile,tableunit="none") {
  ## ----------------
  ##* OPENING REMARK
  ## ----------------
  tbl0       <- cbk.read.casteml(pmlfile,tableunit,category="lead")
  stonelist  <- rownames(tbl0)
  stoneindex <- 1:nrow(tbl0)

  ## ----------------
  ##* PARSE
  ## ----------------
  XX         <- tbl0[,'Pb206zPb204']
  YY1        <- tbl0[,'Pb207zPb204']
  YY2        <- tbl0[,'Pb208zPb204']

  ## ----------------
  ##* PLOT
  ## ----------------
  par(mfrow=c(2,1),oma=c(0.5,0.5,0.5,0.5)) # c(row,col) c(1,1); c(b,l,t,r) c(0,0,0,0)

  # Pb206zPb204_vs_Pb207zPb204
  par(mar=c(2.5,4.5,0.0,0.5)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)
  plot(XX,YY1,type="p",
       col=stoneindex,pch=stoneindex,
       xlab=expression({}^206*"Pb/"*{}^204*"Pb"),ylab=expression({}^207*"Pb/"*{}^204*"Pb"))
  legend("bottomright",stonelist,col=stoneindex,pch=stoneindex,ncol=2,cex=0.5)
  # Northern Hemisphere Reference Line (Hart,1984)
  curve(0.1084*x + 13.491,type="l",lty=1,add=TRUE)

  # Pb206zPb204_vs_Pb208zPb204
  par(mar=c(4.0,4.5,0.0,0.5)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)
  plot(XX,YY2,type="p",
       col=stoneindex,pch=stoneindex,
       xlab=expression({}^206*"Pb/"*{}^204*"Pb"),ylab=expression({}^208*"Pb/"*{}^204*"Pb"))
  legend("bottomright",stonelist,col=stoneindex,pch=stoneindex,ncol=2,cex=0.5)
  # Northern Hemisphere Reference Line (Hart,1984)
  curve(1.209*x + 15.627,type="l",lty=1,add=TRUE)

  ## ----------------
  ##* CLOSING REMARK
  ## ----------------
  return(tbl0)
}
