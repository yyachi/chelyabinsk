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
    cat(file=stderr(),"cbk.lame.texfy1:17: chem # =>",chem,"\n")
  }

  chem_error <- paste0(chem,"_error")
  meanlame   <- pmlame[,chem,drop=FALSE]

  if (verbose) {
    cat(file=stderr(),"cbk.lame.texfy1:23: chem # =>",chem,"\n")
    cat(file=stderr(),"cbk.lame.texfy1:24: chem_error # =>",chem_error,"\n")
  }

  # this is pseudo-errorlame with label "Li7_error" instead of "Li7"
  errorlame  <- pmlame[,chem_error,drop=FALSE]

  if(verbose){
    cat(file=stderr(),"cbk.lame.texfy1:31: meanlame <- ")
    cbk.lame.dump(meanlame)
    cat(file=stderr(),"cbk.lame.texfy1:33: errorlame <- ")
    cbk.lame.dump(errorlame)
  }

  pmlame1    <- cbk.lame.merge.error(meanlame,errorlame,verbose)
  if(verbose){
    cat(file=stderr(),"cbk.lame.texfy1:39: pmlame1 <- ")
    cbk.lame.dump(pmlame1)
  }

  pmlfile    <- cbk.write.casteml(pmlame1)
  if(verbose){
    cat(file=stderr(),"cbk.lame.texfy1:45: pmlfile # =>",pmlfile,"\n")
  }

  texfile    <- cbk.convert.casteml(pmlfile,format="tex")
  if(verbose){
    cat(file=stderr(),"cbk.lame.texfy1:50: texfile # =>",texfile,"\n")
  }

  if (is.null(outfile)) {
    outfile <- texfile
  } else {
    file.copy(texfile,outfile,overwrite=TRUE)
  }
  return(outfile)
}
