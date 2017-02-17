#' @title Return normalized element abundances
#'
#' @description Return normalized element abundances.  Note that only
#'   elements that exist both in pmlame and reflame are processed.
#'   See also "Geochemical Modelling..." by Janousek et al. (2015)
#' @param pmlame A pmlame of element abundances of sample.
#' @param reflame A pmlame of element abundances of reference.
#' @param suffix_after_chem String to recognize column of errors.
#'   Feed "_error" when necessary (default: NULL).
#' @param verbose Output debug info (default: FALSE).
#' @return A ref-normalized daraframe with only elements defined in
#'   ref.
#' @seealso \code{\link{cbk.ref}} and \code{\link{cbk.periodic}}
#' @export
#' @examples
#' pmlame  <- cbk.read.dflame(cbk.path("20081202172326.hkitagawa_trace.dflame"),"ppm")
#' reflame <- cbk.ref("Boynton.1989","ppm")
#' cbk.lame.normalize(pmlame,reflame)
cbk.lame.normalize <- function(pmlame,reflame,suffix_after_chem=NULL,verbose=FALSE){
  ## filter name when number of elements in reflame exceeds those in sample
  ## typically suffix_after_chem is "_error"

  reflame0   <- cbk.lame.regulate(reflame,mean=T,error=F,extra=F)
  meanlame0  <- cbk.lame.regulate(pmlame,mean=T,error=F,extra=F)
  errorlame0 <- cbk.lame.fetch.error(pmlame)

  chem       <- intersect(colnames(reflame0),colnames(meanlame0))

  if(is.null(suffix_after_chem)){
    pmlame1  <- pmlame[,chem]
  } else {
    pmlame1  <- pmlame[,paste0(chem,suffix_after_chem)]
  }
  reflame1   <- cbk.lame.rep(reflame[,chem],nrow(pmlame1))

  if (verbose) {
    cat(file=stderr(),"cbk.lame.normalize:37: chem <-",cbk.lame.dump(chem,show=F),"\n")
    cat(file=stderr(),"cbk.lame.normalize:38: pmlame1 <-",cbk.lame.dump(pmlame1,show=F),"\n")
    cat(file=stderr(),"cbk.lame.normalize:39: reflame1 <-",cbk.lame.dump(reflame1,show=F),"\n")
  }

  pmlame2   <- pmlame1 / reflame1

  if (verbose) {
    cat(file=stderr(),"cbk.lame.normalize:39: pmlame1 <-",cbk.lame.dump(pmlame1,show=F),"\n")
    cat(file=stderr(),"cbk.lame.normalize:40: pmlame2 <-",cbk.lame.dump(pmlame2,show=F),"\n")
  }

  return(pmlame2) # data.frame to be consistent
}
