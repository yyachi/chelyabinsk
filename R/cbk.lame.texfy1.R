#' @title Convert pmlame to texfile
#'
#' @description Convert pmlame to texfile.
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param chem List of chem such as c("Li","Si","Ca","Ca.1","Rb").
#' @param outfile File path to texfile.
#' @param verbose Output debug info.
#' @return File path to texfile.
#' @export
#' @examples
#' pmlame <- cbk.read.casteml(cbk.path("ref_phase_obj.pml"))
#' chem <- c("Li","B","Si","Rb","Sr","Y","Zr","Nb","Cs")
#' cbk.lame.texfy1(pmlame,chem)
cbk.lame.texfy1 <- function(pmlame,chem,outfile=NULL,verbose=FALSE) {
  if (verbose) {
    cat(file=stderr(),"cbk.lame.texfy1:16: chem # =>",chem,"\n")
  }

  ## meanlame   <- pmlame[,chem,drop=FALSE]
  ## chem_error <- paste0(chem,"_error")
  ### The errorlame is with label "Li7" instead of "Li7_error"
  ### Below is incorrect errorlame.
  ## errorlame  <- pmlame[,chem_error,drop=FALSE]

  meanlame      <- cbk.lame.regulate(pmlame,chem=chem,mean=T,error=F,extra=F)
  errorlame     <- cbk.lame.fetch.error(pmlame,chem=chem)

  if(verbose){
    cat(file=stderr(),"cbk.lame.texfy1:28: meanlame <- ")
    cbk.lame.dump(meanlame)
    cat(file=stderr(),"cbk.lame.texfy1:30: errorlame <- ")
    cbk.lame.dump(errorlame)
  }

  pmlame1    <- cbk.lame.merge.error(meanlame,errorlame,verbose)
  if(verbose){
    cat(file=stderr(),"cbk.lame.texfy1:36: pmlame1 <- ")
    cbk.lame.dump(pmlame1)
  }

  pmlfile    <- cbk.write.casteml(pmlame1)
  if(verbose){
    cat(file=stderr(),"cbk.lame.texfy1:42: pmlfile # =>",pmlfile,"\n")
  }

  texfile    <- cbk.convert.casteml(pmlfile,format="tex")
  if(verbose){
    cat(file=stderr(),"cbk.lame.texfy1:47: texfile # =>",texfile,"\n")
  }

  if (is.null(outfile)) {
    outfile <- texfile
  } else {
    file.copy(texfile,outfile,overwrite=TRUE)
  }
  return(outfile)
}
