#' @title Read CASTEML file and create spider diagram
#'
#' @description Read CASTEML file and create spider diagram.  This
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
#' cbk.plot.spider(pmlfile)
cbk.plot.spider <- function(pmlfile,tableunit="ug/g",property="atomicnumber",reference="Wasson.1988") {
  ### ----------------
  ###* OPENING REMARK
  ### ----------------
  tbl0        <- cbk.read.casteml(pmlfile,tableunit,category="trace")
  periodic    <- cbk.periodic()
  ref1        <- cbk.ref(reference,tableunit,cbk.periodic(property))
  stonelist   <- rownames(tbl0)
  stoneindex  <- 1:nrow(tbl0)
  chemlist    <- colnames(tbl0)

  ### ----------------
  ###* PARSE
  ### ----------------
  property0        <- periodic[chemlist,property] # atomicnumber, volatility, compatibility
  names(property0) <- chemlist
  XX0              <- sort(property0)
  XX               <- 1:length(XX0)
  ZZ               <- tbl0[,names(XX0),drop=FALSE]
  CI               <- cbk.vector(ref1[names(XX0)])
  YY               <- t(ZZ) / CI

  ### ----------------
  ###* PLOTS
  ### ----------------
  ## par(mar=c(4.5,4.5,0.5,0.5),mfrow=c(2,1)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)

  matplot(XX,YY,log="y",type="o",lty=1,pch=stoneindex,
          xlab='',ylab='ZZ/CI',axes=FALSE)
  axis(1,at=XX,labels=names(XX0),cex.axis=0.9,las=2)
  axis(2,axTicks(2),axTicks(2))
  abline(h=1,lty=2)
  box(lwd=1)

  ### ----------------
  ###* CLOSING REMARK
  ### ----------------
  return(tbl0)
}
