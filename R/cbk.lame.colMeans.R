#' Return mean value of each element
#' @param pmlame A pmlame with rows of stone and columns of chem
#' @return A pmlame with mean value of each column
#' @export
#' @examples
#' pmlame  <- cbk.read.casteml(cbk.path("20081202172326.hkitagawa.pml"))
#' pmlame0 <- pmlame[,colnames(pmlame) != "sample_id"]
#' pmlame1 <- cbk.lame.colMeans(pmlame0)
cbk.lame.colMeans <- function(pmlame) {
  delimiter <- "[.]"
  spotlist0 <- rownames(pmlame)
  spotlist1 <- unlist(lapply(strsplit(spotlist0,delimiter),'[[',1))
  stonelist <- unique(spotlist1)

  pmlame2 <- NaN
  for(ii in 1:length(stonelist)) {
    matched <- grepl(stonelist[ii],spotlist0)
    pmlame0 <- pmlame[matched,,drop=FALSE]
    pmlame1 <- data.frame(t(colMeans(pmlame0,na.rm=TRUE)))
    pmlame2 <- rbind(pmlame2,pmlame1)
  }
  pmlame2           <- pmlame2[2:nrow(pmlame2),,drop=FALSE] # drop first row
  rownames(pmlame2) <- stonelist

  ## pmlame0 <- data.frame(t(colMeans(pmlame,na.rm=TRUE)))
  return(pmlame2)
}
