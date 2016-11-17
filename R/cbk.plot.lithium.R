#' @title Read CASTEML file and create lithium diagram
#'
#' @description Read CASTEML dataframe and create lithium diagram.
#'   This function does not save the created diagram.  You should
#'   prepare a canvas in advance.
#'
#' @param pmlame A dataframe of element abundances
#' @return Dataframe used to plot the diagram
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.download.casteml(c("-r","20130528105235-594267"))
#' pmlame  <- cbk.read.casteml(pmlfile,tableunit="none")
#' cbk.plot.lithium(pmlame)
cbk.plot.lithium <- function(pmlame) {

  ## ----------------
  ##* Opening remark
  ## ----------------
  ## pmlame     <- cbk.read.casteml(pmlfile,tableunit)
  pmlame1    <- pmlame[,c("d7Li","Li")]
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
