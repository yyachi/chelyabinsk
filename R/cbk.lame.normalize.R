#' @title Return normalized element abundances
#'
#' @description Return normalized element abundances.  Note that only
#'   elements that exist both in pmlame and reflame are processed.  See
#'   also "Geochemical Modelling..." by Janousek et al. (2015)
#' @param pmlame A pmlame of element abundances of sample
#' @param reflame A pmlame of element abundances of reference
#' @param suffix_after_name_of_element String to recognize column of
#'   errors.  Feed "_error" when necessary (default: NULL)
#' @return A ref-normalized daraframe with only elements defined in
#'   ref
#' @seealso \code{\link{cbk.ref}} and \code{\link{cbk.periodic}}
#' @export
#' @examples
#' pmlame  <- cbk.read.dflame(cbk.path("20081202172326.hkitagawa_trace.dflame"),"ppm")
#' reflame <- cbk.ref("Boynton.1989","ppm")
#' cbk.lame.normalize(pmlame,reflame)
cbk.lame.normalize <- function(pmlame,reflame,suffix_after_name_of_element=NULL){
  ## filter name when number of elements in reflame exceeds those in sample
  ## typically suffix_after_name_of_element is "_error"
  names.share            <- intersect(names(reflame),names(pmlame))
  reflame1               <- reflame[names.share]
  ## names(reflame1)     <- names.share

  ## extraction and normalization
  if(is.null(suffix_after_name_of_element)){
    normtbl              <- t(pmlame[,names(reflame1)])/cbk.vector(reflame1)
  } else {
    names_with_suffix    <- paste(names(reflame1),suffix_after_name_of_element,sep="")
    ## normtbl           <- t(pmlame[,names_with_suffix])/cbk.vector(reflame1)
    ## rownames(normtbl) <- names.share
    normtbl              <- t(pmlame[,names(reflame1)])/cbk.vector(reflame1)
    rownames(normtbl)    <- names_with_suffix
  }

  ## return(normtbl)
  return(as.data.frame(t(normtbl))) # data.frame to be consistent
}
