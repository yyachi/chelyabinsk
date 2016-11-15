#' @title Read CASTEML file and create lead diagram
#'
#' @description Read CASTEML file and create lead diagram.  This
#'   function does not save the created diagram.  You should prepare a
#'   canvas in advance.
#'
#' @param pmlame A dataframe of element abundances
#' @param tableunit Unit to toss to cbk.read.casteml()
#' @return Dataframe used to plot the diagram
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' pmlame  <- cbk.read.casteml(pmlfile,category="lead")
#' cbk.plot.lead(pmlame)
cbk.plot.lead <- function(pmlame,tableunit="none") {
  ## ----------------
  ##* OPENING REMARK
  ## ----------------
  ## pmlame     <- cbk.read.casteml(pmlfile,tableunit,category="lead")
  stonelist  <- rownames(pmlame)
  stoneindex <- 1:nrow(pmlame)

  ## ----------------
  ##* PARSE
  ## ----------------
  XX         <- pmlame[,'Pb206zPb204']
  YY1        <- pmlame[,'Pb207zPb204']
  YY2        <- pmlame[,'Pb208zPb204']

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
  return(pmlame)
}
