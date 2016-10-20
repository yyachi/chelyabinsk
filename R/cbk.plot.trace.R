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
#' @return trace diagram
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' cbk.plot.trace(pmlfile)
cbk.plot.trace <- function(pmlfile,tableunit="ug/g",property="atomicnumber",reference="Wasson.1988") {
  ## cbkfile  <- cbk.convert.casteml(pmlfile,category="trace")
  ## tbl0     <- cbk.read.dataframe(cbkfile,tableunit)
  tbl0     <- cbk.read.casteml(pmlfile,tableunit,category="trace")
  periodic <- cbk.periodic()
  ref1     <- cbk.ref(reference,tableunit,cbk.periodic(property))
  stonelist   <- rownames(tbl0)
  stoneindex  <- 1:nrow(tbl0)
  chemlist    <- colnames(tbl0)
  REElist     <- c('La','Ce','Pr','Nd','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu');
  subchemlist <- intersect(REElist,chemlist)

  ### ----------------
  ###* PARSE FULL AND PARTIAL
  ### ----------------

  ### full
  property0        <- periodic[chemlist,property] # atomicnumber, volatility, compatibility
  names(property0) <- chemlist
  XX0              <- sort(property0)
  XX               <- 1:length(XX0)
  ZZ               <- tbl0[,names(XX0),drop=FALSE]
  CI               <- cbk.vector(ref1[names(XX0)])
  YY               <- t(ZZ) / CI

  ### partial
  property1        <- periodic[subchemlist,property] # atomicnumber, volatility, compatibility
  names(property1) <- subchemlist
  XX1              <- sort(property1)
  ZZ1              <- tbl0[,names(XX1),drop=FALSE]
  CI1              <- cbk.vector(ref1[names(XX1)])
  YY1              <- t(ZZ1) / CI1

  ### ----------------
  ###* PLOTS SPIDER AND REE
  ### ----------------
  par(mar=c(4.5,4.5,0.5,0.5),mfrow=c(2,1)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)

  ### full
  matplot(XX,YY,log="y",type="o",lty=1,pch=stoneindex,
          xlab='',ylab='ZZ/CI',axes=FALSE)
  axis(1,at=XX,labels=names(XX0),cex.axis=0.9,las=2)
  axis(2,axTicks(2),axTicks(2))
  abline(h=1,lty=2)
  box(lwd=1)

  ### pertial
  matplot(XX1,YY1,log="y",type="o",lty=1,pch=stoneindex,
          xlab='',ylab='ZZ/CI',axes=FALSE)
  axis(1,at=XX1,labels=names(XX1),cex.axis=0.9,las=2)
  axis(2,axTicks(2),axTicks(2))
  abline(h=1,lty=2)
  box(lwd=1)
  legend('bottomright',stonelist,lty=1,pch=stoneindex,col=stoneindex,ncol=4,cex=0.5)
  ## return(YY)
}
