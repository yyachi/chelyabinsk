#' Return mean value of each element
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @return A pmlame with mean value of each column
#' @export
#' @examples
#' pmlame  <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame0 <- pmlame[,setdiff(colnames(pmlame), c("sample_id","image_id"))]
#' pmlame1 <- cbk.lame.colMeans(pmlame0)
cbk.lame.colMeans <- function(pmlame) {
  ## delim_regexp <- "[@|.]"
  delim_regexp    <- "@|[.]|-[[:alnum:]]*$"
  spotlist0       <- rownames(pmlame)
  spotlist1       <- unlist(lapply(strsplit(spotlist0,delim_regexp),'[[',1))
  stonelist       <- unique(spotlist1)

  pmlame2 <- NaN
  for(ii in 1:length(stonelist)) {
    matched <- grepl(stonelist[ii],spotlist0)
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
