#' Return standard deviation of each chem
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param stonify_regexp A regexp string to stonefy analyses. Matched
#'   string will be stripped off from acqlist to have stonelist.
#' @return A pmlame with standard deviation value of each column
#' @export
#' @examples
#' pmlame  <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame0 <- pmlame[,setdiff(colnames(pmlame), c("sample_id","image_id"))]
#' pmlame1 <- cbk.lame.colSds(pmlame0)
cbk.lame.colSds <- function(pmlame,stonify_regexp="@[[:digit:]]+$") {
  ## stonify_regexp <- "@|[.]|-[[:alnum:]]*$" # until February 4, 2017

  acqlist0    <- rownames(pmlame)
  ## acqlist1    <- unlist(lapply(strsplit(acqlist0,stonify_regexp),'[[',1))
  acqlist1    <- gsub(stonify_regexp,"",acqlist0)
  stonelist    <- unique(acqlist1)

  pmlame2 <- NaN
  for(ii in 1:length(stonelist)) {
    matched <- grepl(stonelist[ii],acqlist0)
    pmlame0 <- pmlame[matched,,drop=FALSE]
    pmlame1 <- data.frame(t(apply(pmlame0, 2, sd,na.rm=T)))
    pmlame2 <- rbind(pmlame2,pmlame1)
  }
  pmlame2           <- pmlame2[2:nrow(pmlame2),,drop=FALSE] # drop first row
  rownames(pmlame2) <- stonelist

  ## pmlame0 <- data.frame(t(apply(pmlame, 2, sd,na.rm=T)))
  return(pmlame2)
}
