#' @title Convert pmlame to texfile
#'
#' @description Convert pmlame to texfile.
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param chem List of chem such as c("Li","Si","Ca","Ca.1","Rb").
#' @param outfile File path to texfile.
#' @return File path to texfile.
#' @export
#' @examples
#' pmlfile <- cbk.path("ref_phase_obj.pml")
#' pmlame <- cbk.read.casteml(pmlfile)
#' chem <- c("Li","B","Si","Rb","Sr","Y","Zr","Nb","Cs")
#' cbk.lame.texfy1(pmlame,chem)
cbk.lame.texfy1 <- function(pmlame,chem,outfile=NULL) {

  chem_error <- paste0(chem,"_error")
  meanlame   <- pmlame[,chem]

  # this is pseudo-errorlame with label "Li7_error" instead of "Li7"
  errorlame  <- pmlame[,chem_error]

  pmlame1    <- cbk.lame.merge.error(meanlame,errorlame)
  pmlfile    <- cbk.write.casteml(pmlame1)
  texfile    <- cbk.convert.casteml(pmlfile,format="tex")

  if (!is.null(outfile)) {
    file.copy(texfile,outfile)
  } else {
    outfile <- texfile
  }
  return(outfile)
}
