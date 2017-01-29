#' @title Convert pmlame to multple texfiles
#'
#' @description Convert pmlame to multple texfiles.  This function
#'   split chem and parse the datasets separatedly.
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param chem List of chem such as c("Li","Si","Ca","Ca.1","Rb").
#' @param outfile File path to texfile.
#' @param ncol Number of columns per a table.
#' @param verbose Output debug info.
#' #' @return Vector of file path to texfiles.
#' @seealso \code{\link{cbk.lame.texfy1}}
#' @export
cbk.lame.texfy <- function(pmlame,chem,outfile,ncol=11,verbose=FALSE) {

  if (verbose) {
    cat(file=stderr(),"cbk.lame.texfy:16: outfile # =>",outfile,"\n")
  }

  outbase  <- tools::file_path_sans_ext(outfile)
  outext   <- tools::file_ext(outfile)
  ## outfile <- paste0(outbase,outext)

  chemlist <- split(chem, ceiling(seq_along(chem)/ncol))

  texfiles <- c()
  for(ii in 1:length(chemlist)) {
    if (ii!=1) {
      outfile <- sprintf("%s-%d.%s",outbase,ii,outext)
    }
    ichem <- unlist(chemlist[ii])
    texfile <- cbk.lame.texfy1(pmlame,ichem,outfile=outfile)
    texfiles <- append(texfiles,texfile)
  }

  return(texfiles)
}
