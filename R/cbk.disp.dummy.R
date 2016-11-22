#' Display dummy diagram
#' @param error A error message to be displayed on diagram
#' @return A dataframe used to plot
#' @export
#' @examples
#' tryCatch({cbk.plot("20081202172326.hkitagawa")
#' },error=function(error){
#' cbk.disp.dummy(error)})
cbk.disp.dummy <- function(error,xlabel=NULL) {
  periodic <- cbk.periodic()
  ref      <- cbk.ref("Wasson.1988","ug/g",cbk.periodic("atomicnumber"))
  if (is.null(xlabel)) {
    XX     <- 1:length(ref)
    xlabel <- names(ref)
  } else {
    XX     <- 1:length(xlabel)
  }
  YY       <- ref / ref
  plot(XX,YY[XX],log="y",type="o",lty=1,pch=4,col="red",
       xlab='',ylab='ZZ/CI',axes=FALSE)
  text(length(XX)/2,2,print(error),cex=0.8)
  axis(1,at=XX,labels=xlabel,cex.axis=0.9,las=2)
  axis(2,axTicks(2),axTicks(2))
  abline(h=1,lty=2)
  box(lwd=1)
  return(ref)
}
