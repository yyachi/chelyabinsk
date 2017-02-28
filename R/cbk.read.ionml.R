#' @title Read IONML file and return an ionlame
#'
#' @param ionml.xml An IONML file created in advance using appropriate
#'   script dedicated for device.
#' @param verbose Output debug info (default: TRUE).
#' @return A dataframe with mean and time columns.
#' @seealso \code{\link{ionml.convert.laicpqms}},
#'   \code{ionml.convert.iontblame}, and
#'   \url{https://github.com/misasa/casteml}
#' @export
#' @examples
#' ionlame    <- cbk.read.ionml(cbk.path("ref_cpx_klb1@1.xml"))
cbk.read.ionml <- function(ionml.xml,verbose=TRUE){
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

  colnames(meanlame0) <- isomeas

  if (all(apply(timelame0,1,sd)==0)) { # if all columns are identical
    ## colnames(timelame0)[1] <- "time"
    ionlame2 <- cbind(time=timelame0[,1],meanlame0)
  } else {
    colnames(timelame0) <- paste0(isomeas,"_time")

    ionlame1 <- cbind(timelame0,meanlame0)
    n        <- ncol(meanlame0)
    index    <- rep(1:n, each = 2) + (0:1) * n
    ionlame2 <- ionlame1[index]
  }
  ionlame3 <- as.data.frame(apply(ionlame2,c(1,2),as.numeric))
  return(ionlame3)
}
