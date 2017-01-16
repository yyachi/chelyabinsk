#' @title Read CASTEML file and create lead diagram
#'
#' @description Read CASTEML dataframe and create lead diagram.  This
#'   function does not save the created diagram.  You should prepare a
#'   canvas in advance.
#'
#' @param pmlfile_or_stone A dataframe of element abundances (or pmlfile or stone-ID).
#' @param opts List of further options for plot.  See
#'   \code{\link{cbk.plot}}.
#' @return A pmlmae used to plot the diagram.
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' cbk.plot.lead(pmlfile)
cbk.plot.lead <- function(pmlfile_or_stone,opts=NULL) {
  opts_default <- list(legendp=TRUE, Recursivep=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)
  ## ----------------
  ##* OPENING REMARK
  ## ----------------
  ## pmlame <- cbk.read.casteml(pmlfile,tableunit,category="lead")
  pmlame0   <- cbk.read.casteml(pmlfile_or_stone,opts)
  pmlame1   <- pmlame0[,c("Pb206zPb204","Pb207zPb204","Pb208zPb204")]
  pmlame1   <- cbk.lame.drop.dharma(pmlame1)

  stonelist  <- rownames(pmlame0)
  stoneindex <- 1:nrow(pmlame0)

  ## ----------------
  ##* PARSE
  ## ----------------
  XX         <- pmlame1[,'Pb206zPb204']
  YY1        <- pmlame1[,'Pb207zPb204']
  YY2        <- pmlame1[,'Pb208zPb204']

  ## ----------------
  ##* PLOT
  ## ----------------
  par(mfrow=c(2,1),oma=c(0.5,0.5,0.5,0.5)) # c(row,col) c(1,1); c(b,l,t,r) c(0,0,0,0)

  # Pb206zPb204_vs_Pb207zPb204
  par(mar=c(2.5,4.5,0.0,0.5)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)
  plot(XX,YY1,type="p",
       col=stoneindex,pch=stoneindex,asp=1,
       xlab=expression({}^206*"Pb/"*{}^204*"Pb"),ylab=expression({}^207*"Pb/"*{}^204*"Pb"))
  if (opts$legendp) {
    legend("bottomright",stonelist,col=stoneindex,pch=stoneindex,ncol=2,cex=0.5)
  }
  # Northern Hemisphere Reference Line (Hart,1984)
  curve(0.1084*x + 13.491,type="l",lty=1,add=TRUE)

  # Pb206zPb204_vs_Pb208zPb204
  par(mar=c(4.0,4.5,0.0,0.5)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)
  plot(XX,YY2,type="p",asp=1,
       col=stoneindex,pch=stoneindex,
       xlab=expression({}^206*"Pb/"*{}^204*"Pb"),ylab=expression({}^208*"Pb/"*{}^204*"Pb"))
  if (opts$legendp) {
    legend("bottomright",stonelist,col=stoneindex,pch=stoneindex,ncol=2,cex=0.5)
  }
  # Northern Hemisphere Reference Line (Hart,1984)
  curve(1.209*x + 15.627,type="l",lty=1,add=TRUE)

  ## ----------------
  ##* CLOSING REMARK
  ## ----------------
  return(pmlame1)
}
