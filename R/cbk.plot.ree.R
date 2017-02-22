#' @title Read CASTEML file and create REE diagram
#'
#' @description Read CASTEML dataframe and create REE diagram.  This
#'   function does not save the created diagram.  You should prepare a
#'   canvas in advance.
#'
#' @param pmlfile_or_stone A CASTEML file that exits locally or
#'   stone-ID (or pmlame).
#' @param tableunit Unit to toss to \link{cbk.read.casteml}.
#' @param reference Reference of element abundance.
#' @param opts List of further options for plot.  See \link{cbk.plot}.
#' @param verbose Output debug info (default: FALSE).
#' @return @return A pmlame used to plot the diagram.
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' cbk.plot.ree(pmlfile)
#'
#' pmlame  <- cbk.read.casteml(cbk.path("20160921173604-511857.pml"))
#' cbk.plot.ree(pmlame)
cbk.plot.ree <- function(pmlfile_or_stone,opts=NULL,tableunit="none",reference="Wasson.1988",verbose=FALSE) {
  ## ----------------
  ##* PARSE OPTION
  ## ----------------
  opts_default <- list(legendp=TRUE, Recursivep=FALSE, pch=FALSE, col=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)

  ## ----------------
  ##* OPENING REMARK
  ## ----------------
  ## pmlame <- cbk.read.casteml(pmlfile,tableunit,category="trace")
  pmlame  <- cbk.read.casteml(pmlfile_or_stone,opts,tableunit)
  reflame <- cbk.ref(reference,tableunit)
  stone   <- rownames(pmlame)

  if (verbose) {
    cat(file=stderr(),"cbk.plot.ree:39: pmlame <-",cbk.lame.dump(pmlame,show=F),"\n")
    cat(file=stderr(),"cbk.plot.ree:40: stone <-",cbk.lame.dump(stone,show=F),"\n")
    cat(file=stderr(),"cbk.plot.ree:41: reflame <-",cbk.lame.dump(reflame,show=F),"\n")
  }

  pmlame1    <- cbk.lame.normalize(pmlame,reflame,verbose=verbose)
  meanlame1  <- cbk.lame.regulate(pmlame1,mean=T,error=F,extra=F)
  errorlame1 <- cbk.lame.fetch.error(pmlame1)

  ## ----------------
  ##* PARSE
  ## ----------------
  chem0      <- intersect(c('La','Ce','Pr','Nd','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu'), colnames(meanlame1));
  XX         <- cbk.periodic("atomicnumber")[chem0]
  chem       <- names(XX)
  if (verbose) {
    cat(file=stderr(),"cbk.plot.ree:55: chem <-",cbk.lame.dump(chem,show=F),"\n")
  }

  ## Deal mean
  meanlame8  <- meanlame1[,chem,drop=FALSE]
  YY         <- t(meanlame8)
  pmlame9    <- meanlame8               # for output

  ## Deal error
  if (length(errorlame1) != 0) {
    if (verbose) {
      cat(file=stderr(),"cbk.plot.ree:66: errorlame1 <-",cbk.lame.dump(errorlame1,show=F),"\n")
    }
    errorlame8 <- errorlame1[,chem,drop=FALSE]
    YY_error   <- t(errorlame8)
    pmlame9    <- cbk.lame.merge.error(meanlame8,errorlame8) # for output
  }

  ## ----------------
  ##* PLOT
  ## ----------------
  if (opts$pch) {
    pch <- opts$pch
  } else {
    pch <- 1:length(stone)
  }
  if (opts$col) {
    col <- opts$col
  } else {
    col  <- 1:length(stone)
  }

  matplot(XX,YY,log="y",type="o",lty=1,pch=pch,col=col,
          xlab='',ylab='ZZ/CI',axes=FALSE)
  if (length(errorlame1) != 0) {
    errorbar.y <- function(XX,YY,err,WW,col=1){x0=XX;y0=YY-err;x1=XX;y1=YY+err;arrows(x0,y0,x1,y1,code=3,angle=90,length=WW,col=col);}
    for(ii in 1:length(stone)) {
      errorbar.y(XX,YY[,ii],YY_error[,ii],0.05,col=col[ii])
    }
  }
  axis(1,at=XX,labels=chem,cex.axis=0.9,las=2)
  axis(2,axTicks(2),axTicks(2))
  abline(h=1,lty=2)
  box(lwd=1)

  if (opts$legendp) {
    legend('bottomright',stone,lty=1,pch=pch,col=col,ncol=4,cex=0.5)
  }

  ## ----------------
  ##* CLOSING REMARK
  ## ----------------
  return(pmlame9)
}
