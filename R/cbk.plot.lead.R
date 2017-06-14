#' @title Read CASTEML file and create lead diagram
#'
#' @description Read CASTEML dataframe and create lead diagram.  This
#'   function does not save the created diagram.  You should prepare a
#'   canvas in advance.
#'
#' @param pmlfile_or_stone A dataframe of element abundances (or pmlfile or stone-ID).
#' @param opts List of further options for plot.  See
#'   \code{\link{cbk.plot}}.
#' @param verbose Output debug info (default: FALSE).
#' @param pch Array of symbol (default: NULL)
#' @param col Array of color (default: NULL)
#' @return A pmlame used to plot the diagram.
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' cbk.plot.lead(pmlfile)
cbk.plot.lead <- function(pmlfile_or_stone,opts=NULL,verbose=FALSE,pch=NULL,col=NULL) {
  opts_default <- list(legendp=TRUE, Recursivep=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)
  ## ----------------
  ##* OPENING REMARK
  ## ----------------
  ## pmlame <- cbk.read.casteml(pmlfile,tableunit,category="lead")
  pmlame0    <- cbk.read.casteml(pmlfile_or_stone,opts)
  pmlame1    <- pmlame0[,c("Pb206zPb204","Pb207zPb204","Pb208zPb204")]
  pmlame1    <- cbk.lame.drop.dharma(pmlame1)

  stone      <- rownames(pmlame1)
  ## stoneindex <- 1:nrow(pmlame1)

  if (verbose) {
    cat(file=stderr(),"cbk.plot.lead:35: pmlame0 <-",cbk.lame.dump(pmlame0,show=F),"\n")
    cat(file=stderr(),"cbk.plot.lead:36: stone <-",cbk.lame.dump(stone,show=F),"\n")
  }

  ## ----------------
  ##* PARSE
  ## ----------------
  XX         <- pmlame1[,'Pb206zPb204']
  YY1        <- pmlame1[,'Pb207zPb204']
  YY2        <- pmlame1[,'Pb208zPb204']

  if (any(c("Pb206zPb204_error","Pb207zPb204_error","Pb208zPb204_error") %in% colnames(pmlame0))) {
    XX_sd      <- pmlame0[stone,"Pb206zPb204_error"]
    YY1_sd     <- pmlame0[stone,"Pb207zPb204_error"]
    YY2_sd     <- pmlame0[stone,"Pb208zPb204_error"]
  }

  ## ----------------
  ##* PLOT
  ## ----------------
  if (is.null(pch)) {
    pch <- 1:length(stone) %% 26
  ## } else {
  ##   pch <- opts$pch
  }
  if (is.null(col)) {
    col  <- 1:length(stone)
  ## } else {
  ##   col <- opts$col
  }

  par(mfrow=c(2,1),oma=c(0.5,0.5,0.5,0.5)) # c(row,col) c(1,1); c(b,l,t,r) c(0,0,0,0)

  # Pb206zPb204_vs_Pb207zPb204
  par(mar=c(2.5,4.5,0.0,0.5)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)
  plot(XX,YY1,type="p",
       col=col,pch=pch,asp=1,
       xlab=expression({}^206*"Pb/"*{}^204*"Pb"),ylab=expression({}^207*"Pb/"*{}^204*"Pb"))
  errorbar.x <- function(XX,YY,err,WW,col=1){x0=XX-err;y0=YY;x1=XX+err;y1=YY;arrows(x0,y0,x1,y1,code=3,angle=90,length=WW,col=col);}
  errorbar.y <- function(XX,YY,err,WW,col=1){x0=XX;y0=YY-err;x1=XX;y1=YY+err;arrows(x0,y0,x1,y1,code=3,angle=90,length=WW,col=col);}
  if (any(c("Pb206zPb204_error","Pb207zPb204_error","Pb208zPb204_error") %in% colnames(pmlame0))) {
    errorbar.x(XX,YY1,XX_sd,0.05,col=col)
    errorbar.y(XX,YY1,YY1_sd,0.05,col=col)
  }
  if (opts$legendp) {
    legend("bottomright",stone,col=col,pch=pch,ncol=2,cex=0.5)
  }
  # Northern Hemisphere Reference Line (Hart,1984)
  curve(0.1084*x + 13.491,type="l",lty=1,add=TRUE)

  # Pb206zPb204_vs_Pb208zPb204
  par(mar=c(4.0,4.5,0.0,0.5)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)
  plot(XX,YY2,type="p",asp=1,
       col=col,pch=pch,
       xlab=expression({}^206*"Pb/"*{}^204*"Pb"),ylab=expression({}^208*"Pb/"*{}^204*"Pb"))
  if (any(c("Pb206zPb204_error","Pb207zPb204_error","Pb208zPb204_error") %in% colnames(pmlame0))) {
    errorbar.x(XX,YY2,XX_sd,0.05,col=col)
    errorbar.y(XX,YY2,YY2_sd,0.05,col=col)
  }
  if (opts$legendp) {
    legend("bottomright",stone,col=col,pch=pch,ncol=2,cex=0.5)
  }
  # Northern Hemisphere Reference Line (Hart,1984)
  curve(1.209*x + 15.627,type="l",lty=1,add=TRUE)

  ## ----------------
  ##* CLOSING REMARK
  ## ----------------
  return(pmlame1)
}
