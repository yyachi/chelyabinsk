#' @title Read CASTEML file and create oxygen diagram
#'
#' @description Read CASTEML dataframe and create oxygen diagram.
#'   This function does not save the created diagram.  You should
#'   prepare a canvas in advance.
#'
#' @param pmlfile_or_stone A CASTEML file that exits locally or stone-ID (or pmlame).
#' @param opts List of further options for plot.  See
#'   \code{\link{cbk.plot}}.
#' @return A pmlame used to plot the diagram.
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.path("20130528105235-594267.pml")
#' cbk.plot.oxygen(pmlfile)
cbk.plot.oxygen <- function(pmlfile_or_stone,opts=NULL) {
  opts_default <- list(legendp=TRUE, Recursivep=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)
  ## ----------------
  ##* OPENING REMARK
  ## ----------------
  ## pmlame <- cbk.read.casteml(pmlfile,tableunit,category="oxygen")
  pmlame0   <- cbk.read.casteml(pmlfile_or_stone,opts,tableunit="permil")
  pmlame1   <- pmlame0[,c("d18O","d17O")]
  pmlame1   <- cbk.lame.drop.dharma(pmlame1)

  stonelist  <- rownames(pmlame1)
  stoneindex <- 1:nrow(pmlame1)

  ## ----------------
  ##* PARSE
  ## ----------------
  XX         <- pmlame1[,'d18O']
  YY         <- pmlame1[,'d17O']

  if (any(c("d18O_error","d17O_error") %in% colnames(pmlame0))) {
    XX_sd     <- pmlame0[stonelist,"d18O_error"]
    YY_sd     <- pmlame0[stonelist,"d17O_error"]
  }

  ## ----------------
  ##* PLOT
  ## ----------------
  plot(XX,YY,type="p",pch=stoneindex,col=stoneindex,
       xlab=expression(paste(delta,{}^18*O)), ylab=expression(paste(delta,{}^17*O)), asp=1)
  if (any(c("d18O_error","d17O_error") %in% colnames(pmlame0))) {
    errorbar.x <- function(XX,YY,err,WW,col=1){x0=XX-err;y0=YY;x1=XX+err;y1=YY;arrows(x0,y0,x1,y1,code=3,angle=90,length=WW,col=col);}
    errorbar.y <- function(XX,YY,err,WW,col=1){x0=XX;y0=YY-err;x1=XX;y1=YY+err;arrows(x0,y0,x1,y1,code=3,angle=90,length=WW,col=col);}

    errorbar.x(XX,YY,XX_sd,0.05,col=stoneindex)
    errorbar.y(XX,YY,YY_sd,0.05,col=stoneindex)
  }
  if (opts$legendp) {
    legend('bottomright',stonelist,ncol=4,cex=0.5,pch=stoneindex,col=stoneindex)
  }
  ## ----------------
  ##* Draw reference lines
  ## ----------------
  abline(0,0.52)  # TF line
  abline(-4,0.94) # CCAM line
  abline(-1.04,1) # YR line
  text(3.7,1.5,"TF")
  text(6.2,1.0,"CCAM")
  text(1.8,0,"Y&R")
  ## CBK bulk
  d18O.CBK <- c(4.88,4.93,5.06,4.73,4.87,4.93,4.96,4.96,4.80,4.78,4.69,4.77,5.09,4.97,5.08,5.16,5.10,4.90,4.43,4.82,4.92,4.81,5.06,5.11,4.76,5.19,5.10,5.13)
  d17O.CBK <- c(3.78,3.83,3.90,3.75,3.81,3.86,3.88,3.83,3.76,3.73,3.65,3.75,3.89,3.86,3.88,3.92,3.90,3.80,3.57,3.75,3.79,3.71,3.83,4.00,3.71,3.95,3.93,3.95)
  points(d18O.CBK, d17O.CBK, pch=3)
  text(4.2,4.0,"CBK")
  ## Itokawa grain
  d18O.itokawa <- c(5.2,2.4,4.0,5.1,7.2,8.0,8.8,2.9,1.7,6.9)
  d17O.itokawa <- c(5.2,2.5,4.6,5.0,5.5,4.2,5.8,2.6,1.7,4.1)
  points(d18O.itokawa, d17O.itokawa, pch=1)
  text(5.2,5.6,"itokawa")

  ## ----------------
  ##* CLOSING REMARK
  ## ----------------
  return(pmlame1)
}
