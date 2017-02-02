#' @title Convert CASTEML file to multple texfiles
#'
#' @description Convert CASTEML file to multple texfiles.  This
#'   function splits chem and parse the datasets separatedly.
#' @param pmlfile_or_stone A CASTEML file that exits locally or
#'   stone-ID (or pmlame).
#' @param outfile File path to texfile.
#' @param chem List of chem such as c("Li","Si","Ca","Ca.1","Rb").
#' @param ncol Number of columns per a table.
#' @param verbose Output debug info.
#' @return Vector of file path to texfiles.
#' @seealso \code{\link{cbk.lame.texfy1}}
#' @export
cbk.texfy <- function(pmlfile_or_stone,outfile="table-auto.tex",chem=NULL,ncol=11,verbose=FALSE) {

  if (verbose) {
    cat(file=stderr(),"cbk.lame.texfy:16: outfile # =>",outfile,"\n")
  }

  ## pmlame <- cbk.lame.regulate(pmlame)
  pmlame <- cbk.lame.regulate(cbk.read.casteml(pmlfile_or_stone))

  if (is.null(chem)) {
    chem <- grep("_error$",colnames(pmlame),value=T,invert=T)
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
    texfile <- cbk.lame.texfy1(pmlame,ichem,outfile=outfile,verbose=verbose)
    texfiles <- append(texfiles,texfile)
  }

  return(texfiles)
}
