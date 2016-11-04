#' @title Download CASTEML file associated with a stone and create
#'   geochemical diagram
#'
#' @description Download CASTEML file associated with a stone and
#'   create geochemical diagram
#'
#' @param stone Unique indentification number of stones in Medusa.
#' @param category Category filter that is passed to `casteml convert'
#' @return Dataframe used to plot the diagram
#' @export
#' @seealso \code{\link{cbk.plot.trace}}
#' @examples
#' cbk.plot("20130318074936-009-271",category="trace")
#' cbk.plot("20081202172326.hkitagawa",category="trace")
#' cbk.plot("20081202172326.hkitagawa",category="lead")
#' cbk.plot("20130528105235-594267",category="oxygen")
cbk.plot <- function(stone,category="trace") {
  pmlfile <- cbk.download.casteml(c("-r", stone))
  ans <- switch(category,
                "trace"  = cbk.plot.trace(pmlfile),
                "oxygen" = cbk.plot.oxygen(pmlfile),
                "lead"   = cbk.plot.lead(pmlfile),
                stop("No action defined"))
  return(ans)
}
