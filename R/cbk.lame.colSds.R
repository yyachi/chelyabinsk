#' Return standard deviation of each chem
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param stonelist_regexp A regexp string to split stone name
#' @return A pmlame with standard deviation value of each column
#' @export
#' @examples
#' pmlame  <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame0 <- pmlame[,setdiff(colnames(pmlame), c("sample_id","image_id"))]
#' pmlame1 <- cbk.lame.colSds(pmlame0)
cbk.lame.colSds <- function(pmlame,stonelist_regexp=NULL) {
  if (is.null(stonelist_regexp)) {
    ## stonelist_regexp <- "@|[.]|-[[:alnum:]]*$" # until February 4, 2017
    stonelist_regexp    <- "@[[:alnum:]]+$"
  }

  spotlist0    <- rownames(pmlame)
  spotlist1    <- unlist(lapply(strsplit(spotlist0,stonelist_regexp),'[[',1))
  stonelist    <- unique(spotlist1)

  pmlame2 <- NaN
  for(ii in 1:length(stonelist)) {
    matched <- grepl(stonelist[ii],spotlist0)
    pmlame0 <- pmlame[matched,,drop=FALSE]
    pmlame1 <- data.frame(t(apply(pmlame0, 2, sd,na.rm=T)))
    pmlame2 <- rbind(pmlame2,pmlame1)
  }
  pmlame2           <- pmlame2[2:nrow(pmlame2),,drop=FALSE] # drop first row
  rownames(pmlame2) <- stonelist

  ## pmlame0 <- data.frame(t(apply(pmlame, 2, sd,na.rm=T)))
  return(pmlame2)
}
