#' @title Convert pmlame to multple texfiles
#'
#' @description Convert pmlame to multple texfiles.  This function
#'   split chem and parse the datasets separatedly.
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param chem List of chem such as c("Li","Si","Ca","Ca.1","Rb").
#' @param ncol Number of columns per a table.
#' @param outfile File path to texfile.
#' @return Vector of file path to texfiles.
#' @seealso \code{\link{cbk.lame.texfy1}}
#' @export
cbk.lame.texfy <- function(pmlame,chem,ncol=11,outfile) {

  outbase  <- tools::file_path_sans_ext(outfile)
  outext   <- tools::file_ext(texfile)
  ## outfile <- paste0(outbase,outext)

  texfiles <- c()
  
  chemlist <- split(chem, ceiling(seq_along(chem)/ncol))

  for(ii in 1:length(chemlist)) {
    if (ii!=1) {
      outfile <- sprintf("%s-%d.%s",outbase,ii,outext)
    }

    ichem <- unlist(chemlist[ii])
    texfile <- cbk.lame.texfy1(pmlame0,ichem,outfile=outfile)
    texfiles <- append(texfiles,texfile)
  }

  return(texfiles)
}
