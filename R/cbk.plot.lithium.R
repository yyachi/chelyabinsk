#' @title Read CASTEML file and create lithium diagram
#'
#' @description Read CASTEML dataframe and create lithium diagram.
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
#' cbk.plot.lithium(pmlfile)
cbk.plot.lithium <- function(pmlfile_or_stone,opts=NULL) {
  opts_default <- list(legendp=TRUE, Recursivep=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)
  ## ----------------
  ##* Opening remark
  ## ----------------
  ## pmlame  <- cbk.read.casteml(pmlfile,tableunit)
  pmlame0    <- cbk.read.casteml(pmlfile_or_stone,opts)
  pmlame1    <- pmlame0[,c("d7Li","Li")]
  pmlame1   <- cbk.lame.drop.dharma(pmlame1)
  XX         <- pmlame1[,'Li']
  YY         <- pmlame1[,'d7Li']

  ## ----------------
  ##* Constant setup
  ## ----------------
  Li7zLi6_0 <- 12.1163 # LSVEC Moriguti and Nakamura, Chem. Geol., 145 (1998), pp. 91--104]
  m6Li      <-  6.0151 # g/mol
  m7Li      <-  7.0163 # g/mol

  ## ----------------
  ##* Plot
  ## ----------------
  plot(XX,YY*1000,xlab="[Li]",ylab=expression(paste(delta,{}^7*Li)),log="x")
  # Really, delta value should be controlled by argument `tableunit'

  ## ----------------
  ##* Closing remark
  ## ----------------
  return(pmlame1)
}
