#' Display message on a diagram.
#' 
#' @param pmlfile_or_stone A CASTEML file that exits locally or
#'   stone-ID (or pmlame)
#' @param text Message to be displayed on diagram.  When it is with
#'   special caractors, quote it in advance.
#' @return Dataframe used to plot the diagram
#' @export
#' @examples
#' pmlame <- cbk.read.casteml(cbk.path("20081202172326.hkitagawa.pml"),"ppm")
#' cbk.plot.message(pmlame,sQuote("Hello, world!"))
cbk.plot.message <- function(pmlfile_or_stone,text) {
  XX    <- 1:100
  ## YY <- rep(1,100)
  YY1    <- 5*1.02^XX*sin(0.995^XX*XX/3)         + 20
  YY2    <- 5*1.02^XX*sin(0.995^XX*XX/3+6.28/3*1) + 20
  YY3    <- 5*1.02^XX*sin(0.995^XX*XX/3+6.28/3*2) + 20
  mylim <- c(0,100)
  par(mar=c(0.1,0.1,0.1,0.1)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)
  plot(XX,YY1,type="l",lty=1,pch=4,col="red",xlim=mylim,ylim=mylim,
       xlab='',ylab='',xaxs='i',yaxs='i',axes=FALSE,lwd=5)
  lines(XX,YY2,type="l",col="blue",lwd=5)
  lines(XX,YY3,type="l",col="green",lwd=5)

  ## text(50,50,text,cex=0.8)
  wrap_line <- function(text,width){
    as.character(sapply(text,FUN=function(xx){paste(strwrap(xx,width=width),collapse="\n")}))
  }
  text(3,97,wrap_line(text,76),adj=c(0,1))

  box(lwd=1)
  return(pmlfile_or_stone)
}
