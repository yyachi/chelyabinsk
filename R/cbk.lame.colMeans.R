#' Return mean of each chem
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param stonify_regexp A regexp string to stonefy analyses. Matched
#'   string will be stripped off from acqlist to have stonelist.
#' @return A pmlame with mean value of each column
#' @export
#' @examples
#' pmlame  <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame0 <- pmlame[,setdiff(colnames(pmlame), c("sample_id","image_id"))]
#' pmlame1 <- cbk.lame.colMeans(pmlame0)
cbk.lame.colMeans <- function(pmlame,stonify_regexp="@[[:digit:]]+$") {
  ## stonify_regexp <- "[@|.]"
  ## stonify_regexp <- "@|[.]|-[[:alnum:]]*$" # until February 4, 2017

  acqlist0       <- rownames(pmlame)
  ## acqlist1       <- unlist(lapply(strsplit(acqlist0,stonify_regexp),'[[',1))
  acqlist1       <- gsub(stonify_regexp,"",acqlist0)
  stonelist       <- unique(acqlist1)

  pmlame2 <- NaN
  for(ii in 1:length(stonelist)) {
    matched <- grepl(stonelist[ii],acqlist0)
    pmlame0 <- pmlame[matched,,drop=FALSE]
    pmlame1 <- data.frame(t(colMeans(pmlame0,na.rm=TRUE)))
    pmlame2 <- rbind(pmlame2,pmlame1)
  }
  pmlame2           <- pmlame2[2:nrow(pmlame2),,drop=FALSE] # drop first row
  rownames(pmlame2) <- stonelist

  pmlame2[pmlame2 == "NaN"] <- NA
  ## pmlame0 <- data.frame(t(colMeans(pmlame,na.rm=TRUE)))
  return(pmlame2)
}
