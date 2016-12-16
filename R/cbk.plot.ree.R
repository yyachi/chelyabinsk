#' @title Read CASTEML file and create REE diagram
#'
#' @description Read CASTEML dataframe and create REE diagram.  This
#'   function does not save the created diagram.  You should prepare a
#'   canvas in advance.
#'
#' @param pmlfile_or_stone A CASTEML file that exits locally or stone-ID (or pmlame)
#' @param tableunit Unit to toss to cbk.read.casteml()
#' @param property Property to align x-axis
#' @param reference Reference of element abundance
#' @param opts List of further options for plot
#' @return Dataframe used to plot the diagram
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' cbk.plot.ree(pmlfile)
cbk.plot.ree <- function(pmlfile_or_stone,opts=NULL,tableunit="ug/g",property="atomicnumber",reference="Wasson.1988") {
  opts_default <- list(legendp=TRUE, axis="equal")
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)
  ## ----------------
  ##* OPENING REMARK
  ## ----------------
  ## pmlame   <- cbk.read.casteml(pmlfile,tableunit,category="trace")
  pmlame      <- cbk.read.casteml(pmlfile_or_stone,tableunit)
  periodic    <- cbk.periodic()
  ref1        <- cbk.ref(reference,tableunit,cbk.periodic(property))
  stonelist   <- rownames(pmlame)
  stoneindex  <- 1:length(stonelist)
  chemlist    <- colnames(pmlame)
  REElist     <- c('La','Ce','Pr','Nd','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu');
  subchemlist <- intersect(REElist,chemlist)

  ## ----------------
  ##* PARSE
  ## ----------------
  property0        <- periodic[subchemlist,property] # atomicnumber, volatility, compatibility
  names(property0) <- subchemlist
  XX0              <- sort(property0)
  ZZ               <- pmlame[,names(XX0),drop=FALSE]
  CI               <- cbk.vector(ref1[names(XX0)])
  YY               <- t(ZZ) / CI

  ## ----------------
  ##* PLOT
  ## ----------------
  ## par(mar=c(4.5,4.5,0.5,0.5),mfrow=c(2,1)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)

  matplot(XX0,YY,log="y",type="o",lty=1,pch=stoneindex,
          xlab='',ylab='ZZ/CI',axes=FALSE)
  axis(1,at=XX0,labels=names(XX0),cex.axis=0.9,las=2)
  axis(2,axTicks(2),axTicks(2))
  abline(h=1,lty=2)
  box(lwd=1)
  if (opts$legendp) {
    legend('bottomright',stonelist,lty=1,pch=stoneindex,col=stoneindex,ncol=4,cex=0.5)
  }
  ## ----------------
  ##* CLOSING REMARK
  ## ----------------
  return(pmlame)
}
