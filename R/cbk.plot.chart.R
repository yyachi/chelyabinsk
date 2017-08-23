#' @title Visualze IONML as intensity chart
#'
#' @description Visualze IONML as intensity chart.  This reads IONML
#'   or ion-type pmlame (R object) and plots intensity chart of
#'   elements.  Two intensity charts will be droawn with and without
#'   normalization by reference ion intensity.
#'
#' @param IONML_or_pmlame IONML (xml file) or ion-type pmlame (R object).
#' @param t2 When ion starts (default: 25 s).
#' @param t3 When ion ends (default: 60 s).
#' @param ref Reference ion species (default: "int_29Si").
#' @param acq Name of analysis such as `ref_cpx_klb1@1' (default: "").
#' @param w_size Window size for moving average (default: 10).
#' @param opts List of further options for plot.  See \link{cbk.plot}.
#' @param verbose Output debug info (default: FALSE).
#' @param ... Graphical options that are passed to matplot and legend,
#'   such as `col' and `pch'.
#' @return @return A pmlame used to plot the diagram.
#' @export
#' @examples
#' ionml.xml <- cbk.path("ref_cpx_klb1@1.xml")
#' cbk.plot.chart(ionml.xml,acq="ref_cpx_klb1@1")
#'
#' pmlame <- cbk.read.ionml(ionml.xml,representative_time=TRUE)
#' cbk.plot.chart(pmlame)
cbk.plot.chart <- function(IONML_or_pmlame,opts=NULL,t2=25,t3=60,ref="int_29Si",acq="",w_size=10,verbose=FALSE,...) {
  library(dplyr)
  library(RcppRoll)
  ## ----------------
  ##* PARSE OPTION
  ## ----------------
  opts_default <- list(legendp=TRUE, Recursivep=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)

  ## ----------------
  ##* OPENING REMARK
  ## ----------------
  if(is.data.frame(IONML_or_pmlame)) {
    pmlame0 <- IONML_or_pmlame
  } else {
    pmlame0 <- cbk.read.ionml(IONML_or_pmlame,representative_time=TRUE)
  }
  pmlame1 <- subset(pmlame0,time>=t2 & time<=t3)

  pmlame0[pmlame0 <= 0] <- NA
  pmlame1[pmlame1 <= 0] <- NA

  if (verbose) {
    cat(file=stderr(),"cbk.plot.chart:49: pmlame <-",cbk.lame.dump(pmlame,show=F),"\n")
  }

  ## ----------------
  ##* PARSE
  ## ----------------
  XX      <- pmlame0["time"]
  YY      <- pmlame0[colnames(pmlame0)!="time"]
  XX1     <- pmlame1["time"]
  YY1     <- pmlame1[colnames(pmlame0)!="time"]
  YY1zref <- YY1 / YY1[,ref]
  ## YYzref  <- YY / YY[,ref]
  chem    <- colnames(YY)
  chem0   <- gsub("int_","",chem)
  ## col     <- rainbow(length(chem))
  ## w_size  <- 10

  if (verbose) {
    cat(file=stderr(),"cbk.plot.chart:67: chem <-",cbk.lame.dump(chem,show=F),"\n")
  }

  ## ----------------
  ##* PLOT
  ## ----------------
  ## Setup args for matplot and legend
  args_auto    <- list(...)
  args_default <- list(col=rainbow(length(chem)),lty=1,type="l",xlab="Time",ylab="intensity",log="y",ncol=3)
  args_default[intersect(names(args_default),names(args_auto))] <- NULL  ## Reset shared args
  args_auto    <- c(args_auto,args_default)
  col          <- args_auto$col

  names_args_func_plot   <- c(names(formals(plot.default)), names(par()))
  names_args_func_legend <- names(formals(legend))

  ## Draw profile of intensity
  par(mar=c(3,3,1.5,0.1))
  par(mfrow=c(2,1))       # c(row,col) c(1,1); c(b,l,t,r) c(0,0,0,0) ,oma=c(0.5,0.0,0.5,0.0)
  par(mgp=c(2.0,0.8,0))   # c(title,label,line) c(3,1,0)
  do.call("matplot",
          c(list(XX,YY,main=acq), args_auto[names(args_auto) %in% names_args_func_plot]))
  ## matplot(XX,YY,type="l",col=col,lty=1,xlab="Time",ylab="Intensity",log="y",main=iacq)
  abline(v=t2)
  abline(v=t3)
  ## text(t2:(t2+length(chem)-1),colMeans(YY,na.rm=T),chem,col=col,cex=0.8)

  ## legend("topleft",chem0,col=col,lty=1,ncol=3,cex=0.5)
  if (opts$legendp) {
    do.call("legend",
            c(list("topleft",chem0,cex=0.5),
              args_auto[names(args_auto) %in% names_args_func_legend]))
  }

  ## Draw profile of intensity normalized by Si
  ## do.call("matplot",
  ##         c(list(XX1,YY1zref,type="n",ylab="Normalized",xlim=c(t2,t3)), args_auto[names(args_auto) %in% names_args_func_plot]))
  matplot(XX1,YY1zref,type="n",col=col,lty=1,xlab="Time",ylab="intensity/ref",log="y",xlim=c(t2,t3))
  text(rep(t3,length(chem)),colMeans(YY1zref,na.rm=T),chem0,col=col,cex=0.8,font=1)

  YY1zref_MovAve <- mutate_all(YY1zref, funs(roll_mean(., n=w_size, fill=NA))) # data smoothing by moving average method
  matlines(XX1,YY1zref_MovAve,type="l",col=col,lty=1)


  ## ----------------
  ##* CLOSING REMARK
  ## ----------------
  return(YY)
}
