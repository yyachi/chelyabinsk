#' Display a diagram with message
#' 
#' @param pmlfile_or_stone A CASTEML file that exits locally or stone-ID (or pmlame)
#' @param msg Message to be displayed on diagram
#' @return Dataframe used to plot the diagram
#' @export
#' @examples
#' pmlame <- cbk.read.casteml(cbk.path("20081202172326.hkitagawa.pml"),"ppm")
#' cbk.plot.message(pmlame,"Hello, world!")
cbk.plot.message <- function(pmlfile_or_stone,msg) {
  XX    <- 1:100
  ## YY <- rep(1,100)
  YY1    <- 5*sin(XX/5)   + 10
  YY2    <- 5*sin(XX/5+1) + 10
  YY3    <- 5*sin(XX/5+2) + 10
  mylim <- c(0,100)
  par(mar=c(0.1,0.1,0.1,0.1)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)
  plot(XX,YY1,type="l",lty=1,pch=4,col="red",xlim=mylim,ylim=mylim,
       xlab='',ylab='',xaxs='i',yaxs='i',axes=FALSE)
  lines(XX,YY2,type="l",col="blue")
  lines(XX,YY3,type="l",col="green")

  ## text(50,50,msg,cex=0.8)
  wrap_strings <- function(vector_of_strings,width){
    as.character(sapply(vector_of_strings,FUN=function(x){paste(strwrap(x,width=width), collapse="\n")}))
  }
  text(3,97,wrap_strings(msg,40),adj=c(0,1))

  box(lwd=1)
  return(pmlfile_or_stone)
}
