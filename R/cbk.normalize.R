#' @title Return normalized element abundances
#'
#' @description Return normalized element abundances.  Note that only
#'   elements that exist both in cbktbl and ref are processed.  See
#'   also "Geochemical Modelling..." by Janousek et al. (2015)
#' @param cbktbl A dataframe of element abundances
#' @param ref A numeric vector of element abundances with label
#' @param suffix_after_name_of_element String to recognize column of
#'   errors.  Feed "_error" when necessary (default: NULL)
#' @return A ref-normalized daraframe with only elements defined in
#'   ref
#' @seealso \code{\link{cbk.ref}}, \code{\link{cbk.periodic}}
#' @export
#' @examples
#' cbk.normalize(cbk.read.dataframe(cbk.path("20130528105235-594267_trace.dataframe"),"ppm"),cbk.ref("Boynton.1989"))
cbk.normalize <- function(cbktbl,ref,suffix_after_name_of_element=NULL){
  ### EXAMPLES
  ### > property <- cbk.periodic("atomicnumber")
  ### > ref      <- cbk.ref("Boynton.1989")
  ### > sample   <- read.table("sazava.tsv",sep="\t",header=T,row.names=1)
  ### >
  ### > yyy      <- cbk.normalize(sample,ref) # normalized values
  ### > xxx      <- property[names(ref)]
  ### >
  ### > plot(xxx,yyy[,"Po-1"],type="o",log="y",axes=FALSE,xlab="",ylab="REE/chondrite",ylim=c(0.1,100),col="darkgreen")
  ### > axis(1,xxx,labels=names(xxx),cex.axis=0.75)
  ### > axis(2,cex.axis=0.75)
  ### > points(xxx,yyy[,"Po-4"],col="blue")
  ### > lines(xxx,yyy[,"Po-4"],col="blue")
  ### > abline(h=(10^(-1:3)),lty="dashed") # grid
  ### > box() # bounding box

  ## name filter when number of elements in ref exceeds those in sample
  ## typically suffix_after_name_of_element is "_error"
  names.share  <- intersect(names(ref),names(cbktbl))
  ref1         <- ref[names.share]
  names(ref1)  <- names.share

  ## extraction and normalization
  if(is.null(suffix_after_name_of_element)){
    normtbl      <- t(cbktbl[,names(ref1)])/ref1
  } else {
    names_with_suffix  <- paste(names(ref1),suffix_after_name_of_element,sep="")
    normtbl           <- t(cbktbl[,names_with_suffix])/ref1
    rownames(normtbl) <- names.share
  }

  ## return(normtbl)
  return(as.data.frame(normtbl)) # data.frame to be consistent
}
