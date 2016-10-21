#' @title Download CASTEML file and create geochemical diagram
#'
#' @description Download CASTEML file and create geochemical diagram
#'
#' @param stone Unique indentification number of stones in Medusa.
#' @param category Category filter that is passed to `casteml convert'
#' @return Dataframe used to plot the diagram
#' @export
#' @seealso \code{\link{cbk.plot.trace}}
#' @examples
#' cbk.plot("20081202172326.hkitagawa",category="trace")
cbk.plot <- function(stone,category="trace") {
  pmlfile <- cbk.download.casteml(stone)
  return(cbk.plot.trace(pmlfile))
}
