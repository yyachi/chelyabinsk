#' Display dummy diagram
#' @param msg A error message to be displayed on diagram
#' @return A dataframe used to plot
#' @export
#' @examples
#' pmlame <- cbk.read.casteml(cbk.path("20081202172326.hkitagawa.pml"),"ppm")
#' tryCatch({plot(XX,YY)
#' },error=function(e){
#' cbk.plot.message(pmlame,e)})
cbk.plot.message <- function(pmlame,msg) {
  ## periodic <- cbk.periodic()
  ## ref      <- cbk.ref("Wasson.1988","ug/g",cbk.periodic("atomicnumber"))
  ## XX       <- 1:length(ref)
  ## xlabel   <- names(ref)
  ## YY       <- ref / ref
  XX   <- 1:100
  YY   <- rep(1,100)
  ylim <- c(0,100)
  plot(XX,YY,log="",type="o",lty=1,pch=4,col="red",ylim=ylim,
       xlab='',ylab='',xaxs='i',yaxs='i',axes=FALSE)
  text(50,50,msg,cex=0.8)
  ## axis(1,at=XX,labels=xlabel,cex.axis=0.9,las=2)
  ## axis(2,axTicks(2),axTicks(2))
  ## abline(h=1,lty=2)
  box(lwd=1)
  return(pmlame)
}
