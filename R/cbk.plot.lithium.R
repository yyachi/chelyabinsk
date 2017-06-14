#' @title Read CASTEML file and create lithium diagram
#'
#' @description Read CASTEML dataframe and create lithium diagram.
#'   This function does not save the created diagram.  You should
#'   prepare a canvas in advance.
#'
#' @param pmlfile_or_stone A CASTEML file that exits locally or stone-ID (or pmlame).
#' @param opts List of further options for plot.  See
#'   \code{\link{cbk.plot}}.
#' @param verbose Output debug info (default: FALSE).
#' @param pch Array of symbol (default: NULL)
#' @param col Array of color (default: NULL)
#' @return @return A pmlame used to plot the diagram.
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.path("20130528105235-594267.pml")
#' cbk.plot.lithium(pmlfile)
cbk.plot.lithium <- function(pmlfile_or_stone,opts=NULL,verbose=FALSE,pch=NULL,col=NULL) {
  opts_default <- list(legendp=TRUE, Recursivep=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)
  ## ----------------
  ##* Opening remark
  ## ----------------
  ## pmlame  <- cbk.read.casteml(pmlfile,tableunit)
  pmlame0    <- cbk.read.casteml(pmlfile_or_stone,opts)
  pmlame1    <- pmlame0[,c("d7Li","Li")]
  pmlame1    <- cbk.lame.drop.dharma(pmlame1)
  XX         <- pmlame1[,'Li']
  YY         <- pmlame1[,'d7Li']
  stone      <- rownames(pmlame1)

  if (verbose) {
    cat(file=stderr(),"cbk.plot.lithium:35: pmlame0 <-",cbk.lame.dump(pmlame0,show=F),"\n")
    cat(file=stderr(),"cbk.plot.lithium:36: stone <-",cbk.lame.dump(stone,show=F),"\n")
  }

  if (any(c("d7Li_error","Li_error") %in% colnames(pmlame0))) {
    XX_sd      <- pmlame0[stone,'Li_error']
    YY_sd      <- pmlame0[stone,'d7Li_error']
  }

  ## ----------------
  ##* Constant setup
  ## ----------------
  Li7zLi6_0 <- 12.1163 # LSVEC Moriguti and Nakamura, Chem. Geol., 145 (1998), pp. 91--104]
  m6Li      <-  6.0151 # g/mol
  m7Li      <-  7.0163 # g/mol

  ## ----------------
  ##* Plot
  ## ----------------
  if (is.null(pch)) {
    pch <- 1:length(stone) %% 26
  ## } else {
  ##   pch <- pch
  }
  if (is.null(col)) {
    col  <- 1:length(stone)
  ## } else {
  ##   col <- col
  }

  plot(XX,YY*1000,xlab="[Li]",ylab=expression(paste(delta,{}^7*Li)),pch=pch,col=col,log="x")
  # Really, delta value should be controlled by argument `tableunit'
  if (any(c("d7Li_error","Li_error") %in% colnames(pmlame0))) {
    errorbar.x <- function(XX,YY,err,WW,col=1){x0=XX-err;y0=YY;x1=XX+err;y1=YY;arrows(x0,y0,x1,y1,code=3,angle=90,length=WW,col=col);}
    errorbar.y <- function(XX,YY,err,WW,col=1){x0=XX;y0=YY-err;x1=XX;y1=YY+err;arrows(x0,y0,x1,y1,code=3,angle=90,length=WW,col=col);}

    errorbar.x(XX,YY*1000,XX_sd,0.05,col=col)
    errorbar.y(XX,YY*1000,YY_sd*1000,0.05,col=col)
  }
  if (opts$legendp) {
    legend('bottomright',stone,lty=0,pch=pch,col=col,ncol=4,cex=0.5)
  }
  ## ----------------
  ##* Closing remark
  ## ----------------
  return(pmlame1)
}
