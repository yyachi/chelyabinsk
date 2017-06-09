#' Read IONML file and return an ionlame
#'
#' @param ionml.xml An IONML file created in advance using appropriate
#'   script dedicated for device.
#' @param representative_time Flag to compress timelame into single
#'   column (default: FALSE).
#' @param verbose Output debug info (default: TRUE).
#' @return A dataframe with mean and time columns.
#' @seealso \code{\link{ionml.convert.qtegracsv2ioncsv}},
#'   \code{ionml.convert.ioncsv}, and
#'   \url{https://github.com/misasa/casteml}
#' @export
#' @examples
#' ionlame    <- cbk.read.ionml(cbk.path("ref_cpx_klb1@1.xml"))
cbk.read.ionml <- function(ionml.xml,representative_time=FALSE,verbose=TRUE){
  library(XML)

  if (verbose) {
    cat(file=stderr(),"cbk.read.ionml:17: ionml.xml # =>",ionml.xml,"\n")
  }

  doc   <- xmlParse(ionml.xml)
  nodes <- getNodeSet(doc, "//target")

  meanlame0 <- as.data.frame(sapply(nodes,xpathSApply,"./data",xmlValue),row.names=TRUE)
  timelame0 <- as.data.frame(sapply(nodes,xpathSApply,"./data",xmlAttrs),row.names=TRUE)
  isomeas   <- sapply(nodes,xpathSApply,"./name",xmlValue)
  ## isomeas   <- gsub("^int_","",isomeas)

  meanlame1 <- as.data.frame(apply(meanlame0,c(1,2),as.numeric))
  timelame1 <- as.data.frame(apply(timelame0,c(1,2),as.numeric))

  colnames(meanlame1) <- isomeas

  ## if (all(apply(timelame0,1,sd)==0)) {
  if (representative_time) {
    ## ionlame2 <- cbind(time=timelame0[,1],meanlame0)
    ionlame2 <- cbind(time=rowMeans(timelame1),meanlame1)
  } else {
    colnames(timelame1) <- paste0(isomeas,"_time")

    ionlame1 <- cbind(timelame1,meanlame1)
    n        <- ncol(meanlame1)
    index    <- rep(1:n, each = 2) + (0:1) * n
    ionlame2 <- ionlame1[index]
  }
  ## ionlame3 <- as.data.frame(apply(ionlame2,c(1,2),as.numeric))
  return(ionlame2)
}
