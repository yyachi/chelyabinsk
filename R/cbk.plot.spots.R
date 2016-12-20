#' @title Read CASTEML file and create spots diagram
#'
#' @description Read CASTEML dataframe and create spots diagram.
#'   This function does not save the created diagram.  You should
#'   prepare a canvas in advance.
#'
#' @param pmlfile_or_stone A CASTEML file that exits locally or stone-ID (or pmlame)
#' @param opts List of further options for plot
#' @return Dataframe used to plot the diagram
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.path("20130528105235-594267.pml")
#' cbk.plot.spots(pmlfile)
cbk.plot.spots <- function(pmlfile_or_stone,opts=NULL) {
  opts_default <- list(legendp=TRUE, Recursivep=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)
  ## ----------------
  ##* OPENING REMARK
  ## ----------------
  ## pmlame <- cbk.read.casteml(pmlfile,tableunit,category="oxygen")
  pmlame0   <- cbk.read.casteml(pmlfile_or_stone)
  pmlame1   <- pmlame0[,c("x_image","y_image")]
  pmlame1   <- cbk.filter.drop.dharma(pmlame1)

  ## stonelist  <- rownames(pmlame1)
  ## stoneindex <- 1:nrow(pmlame1)

  ## ----------------
  ##* PARSE
  ## ----------------
  XX         <- pmlame1[,'x_image']
  YY         <- pmlame1[,'y_image']

  ## ----------------
  ##* PLOT
  ## ----------------
  plot(XX,YY,type="p",xlim=c(-50,50),ylim=c(-50,50),
       ## pch=stoneindex,col=stoneindex,
       xlab="",ylab="",asp=1)
  ## if (opts$legendp) {
  ##   legend('bottomright',stonelist,ncol=4,cex=0.5,pch=stoneindex,col=stoneindex)
  ## }
  ## ----------------
  ##* CLOSING REMARK
  ## ----------------
  return(pmlame1)
}
