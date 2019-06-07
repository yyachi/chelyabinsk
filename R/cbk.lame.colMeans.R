#' Return mean of each chem
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param stonify_regexp A regexp string to be stripped off to stonefy
#'   analyses. Matched string will be stripped off from acqlist to
#'   have stonelist.
#' @return A pmlame with mean value of each column
#' @export
#' @examples
#' pmlfile <- cbk.path("20130528105235-594267.pml")
#' message(sprintf("The pmlfile is located at |%s|.",pmlfile))
#' pmlame  <- cbk.read.casteml(pmlfile)
#' pmlame0 <- cbk.lame.regulate(pmlame,mean=TRUE,error=FALSE,extra=FALSE)
#' pmlame1 <- cbk.lame.colMeans(pmlame0)
cbk.lame.colMeans <- function(pmlame,stonify_regexp="@[[:digit:]]+$") {
  ## stonify_regexp <- "[@|.]"
  ## stonify_regexp <- "@|[.]|-[[:alnum:]]*$" # until February 4, 2017

  acqlist0    <- rownames(pmlame)
  ## acqlist1 <- unlist(lapply(strsplit(acqlist0,stonify_regexp),'[[',1))
  acqlist1    <- gsub(stonify_regexp,"",acqlist0)
  stonelist   <- unique(acqlist1)

  pmlame2 <- data.frame()
  for(ii in 1:length(stonelist)) {
    matched <- grepl(stonelist[ii],acqlist0)
    pmlame0 <- pmlame[matched,,drop=FALSE]
    pmlame1 <- data.frame(t(colMeans(pmlame0,na.rm=TRUE)))
    pmlame2 <- rbind(pmlame2,pmlame1)
  }
  rownames(pmlame2) <- stonelist

  ## pmlame0 <- data.frame(t(colMeans(pmlame,na.rm=TRUE)))
  return(pmlame2)
}
