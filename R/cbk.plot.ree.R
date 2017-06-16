#' @title Read CASTEML file and create REE diagram
#'
#' @description Read CASTEML dataframe and create REE diagram.  This
#'   function tris to blow errorbar.  If you do not see errorbar,
#'   check pmlame that you feed.
#'
#' @param pmlfile_or_stone A CASTEML file that exits locally or
#'   stone-ID (or pmlame).
#' @param tableunit Unit to toss to \link{cbk.read.casteml}.
#' @param reference Reference of element abundance.
#' @param opts List of further options for plot.  See \link{cbk.plot}.
#' @param verbose Output debug info (default: FALSE).
#' @param ... Graphical options that are passed to matplot and legend,
#'   such as `col' and `pch'.
#' @return @return A pmlame used to plot the diagram.
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' cbk.plot.ree(pmlfile)
#'
#' pmlame  <- cbk.read.casteml(cbk.path("20160921173604-511857.pml"))
#' cbk.plot.ree(pmlame)
cbk.plot.ree <- function(pmlfile_or_stone,opts=NULL,tableunit="none",reference="Wasson.1988",verbose=FALSE,...) {
  ## ----------------
  ##* PARSE OPTION
  ## ----------------
  opts_default <- list(legendp=TRUE, Recursivep=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)

  ## ----------------
  ##* OPENING REMARK
  ## ----------------
  ## pmlame <- cbk.read.casteml(pmlfile,tableunit,category="trace")
  pmlame  <- cbk.read.casteml(pmlfile_or_stone,opts,tableunit)
  reflame <- cbk.ref(reference,tableunit)
  ## stone   <- rownames(pmlame)

  if (verbose) {
    cat(file=stderr(),"cbk.plot.ree:41: pmlame <-",cbk.lame.dump(pmlame,show=F),"\n")
    ## cat(file=stderr(),"cbk.plot.ree:40: stone <-",cbk.lame.dump(stone,show=F),"\n")
    cat(file=stderr(),"cbk.plot.ree:43: reflame <-",cbk.lame.dump(reflame,show=F),"\n")
  }

  pmlame1    <- cbk.lame.normalize(pmlame,reflame,verbose=verbose)
  pmlame2    <- cbk.lame.drop.dharma(cbk.lame.reduce(pmlame1))
  stone      <- rownames(pmlame2)
  meanlame1  <- cbk.lame.regulate(pmlame2,mean=T,error=F,extra=F)
  errorlame1 <- cbk.lame.fetch.error(pmlame2)

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
  ## Setup args for matplot and legend
  args_auto    <- list(...)
  args_default <- list(pch=1:length(stone) %% 26,col=1:length(stone),lty=1,log="y",type="o",xlab="",ylab="ZZ/CI",axes=FALSE,ncol=4)
  args_default[intersect(names(args_default),names(args_auto))] <- NULL  ## Reset shared args
  args_auto    <- c(args_auto,args_default)
  col          <- args_auto$col

  names_args_func_plot   <- c(names(formals(plot.default)), names(par()))
  names_args_func_legend <- names(formals(legend))
  do.call("matplot",
          c(list(XX,YY), args_auto[names(args_auto) %in% names_args_func_plot]))
  ## matplot(XX,YY,log="y",type="o",lty=1,pch=pch,col=col,
  ##         xlab='',ylab='ZZ/CI',axes=FALSE)
  if (length(errorlame1) != 0) {
    errorbar.y <- function(XX,YY,err,WW,col=1){x0=XX;y0=YY-err;x1=XX;y1=YY+err;arrows(x0,y0,x1,y1,code=3,angle=90,length=WW,col=col);}
    col        <- rep_len(col,length(stone))
    for(ii in 1:length(stone)) {
      errorbar.y(XX,YY[,ii],YY_error[,ii],0.05,col=col[ii])
    }
  }
  axis(1,at=XX,labels=chem,cex.axis=0.9,las=2)
  axis(2,axTicks(2),axTicks(2))
  abline(h=1,lty=2)
  box(lwd=1)

  if (opts$legendp) {
    do.call("legend",
            c(list('bottomright',stone,cex=0.5),
              args_auto[names(args_auto) %in% names_args_func_legend]))
    ## legend('bottomright',stone,lty=1,pch=pch,col=col,ncol=4,cex=0.5)
  }

  ## ----------------
  ##* CLOSING REMARK
  ## ----------------
  return(pmlame9)
}
