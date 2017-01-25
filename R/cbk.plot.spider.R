#' @title Read CASTEML file and create spider diagram
#'
#' @description Read CASTEML dataframe and create spider diagram.
#'   This function does not save the created diagram.  You should
#'   prepare a canvas in advance.
#'
#' @param pmlfile_or_stone A CASTEML file that exits locally or
#'   stone-ID (or pmlame).
#' @param tableunit Unit to toss to \link{cbk.read.casteml}.
#' @param property Property to align x-axis to pass to \code{\link{cbk.periodic}}.
#' @param reference Reference of element abundance
#' @param opts List of further options for plot.  See
#'   \code{\link{cbk.plot}}.
#' @return A pmlame used to plot the diagram.
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' pmlame  <- cbk.read.casteml(pmlfile,tableunit="ug/g",category=NULL)
#' cbk.plot.spider(pmlame)
cbk.plot.spider <- function(pmlfile_or_stone,opts=NULL,tableunit="none",property="atomicnumber",reference="Wasson.1988") {
  ## ----------------
  ##* PARSE OPTION
  ## ----------------
  opts_default <- list(legendp=TRUE, Recursivep=FALSE, pch=FALSE, col=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)

  ### ----------------
  ###* OPENING REMARK
  ### ----------------
  ## pmlame  <- cbk.read.casteml(pmlfile,tableunit,category=NULL)
  pmlame0    <- cbk.read.casteml(pmlfile_or_stone,opts,tableunit)
  stonelist  <- rownames(pmlame0)

  reflame    <- cbk.ref(reference,tableunit)
  pmlame1    <- cbk.lame.drop.dharma(cbk.lame.reduce(pmlame0))
  pmlame2    <- cbk.lame.normalize(pmlame1,reflame)

  ## ----------------
  ##* PARSE
  ## ----------------
  chemlist   <- colnames(pmlame2)
  XX0        <- sort(cbk.periodic(property)[chemlist])
  pmlame9    <- pmlame2[,names(XX0),drop=FALSE]

  XX         <- 1:length(XX0)
  names(XX)  <- names(XX0)

  YY         <- t(pmlame9)

  ## ----------------
  ##* PLOTS
  ## ----------------
  if (opts$pch) {
    pch <- opts$pch
  } else {
    pch <- 1:length(stonelist)
  }
  if (opts$col) {
    col <- opts$col
  } else {
    col  <- 1:length(stonelist)
  }

  matplot(XX,YY,log="y",type="o",lty=1,pch=pch,col=col,
          xlab='',ylab='ZZ/CI',axes=FALSE)
  axis(1,at=XX,labels=names(XX0),cex.axis=0.9,las=2)
  axis(2,axTicks(2),axTicks(2))
  abline(h=1,lty=2)
  box(lwd=1)
  if (opts$legendp) {
    legend('bottomright',stonelist,lty=1,pch=pch,col=col,ncol=4,cex=0.5)
  }

  ### ----------------
  ###* CLOSING REMARK
  ### ----------------
  return(pmlame9)
}
