#' @title Plot Analyses with descendants of a stone
#'
#' @description Plot Analyses with descendants of a stone.  This
#'   function downloads CASTEML file associated with a stone and
#'   creates geochemical diagram depending on category specified.
#'
#' @param stone Unique indentification number of stone in Medusa.
#'   This will be passed to cbk.download.casteml() with "--recursive"
#'   option.
#' @param category Category filter that is passed to `casteml convert'
#' @return Dataframe used to plot the diagram
#' @export
#' @seealso \code{\link{cbk.plot.trace}}
#' @examples
#' cbk.plot("20130318074936-009-271",category="trace")
#' cbk.plot("20081202172326.hkitagawa",category="trace")
#' cbk.plot("20130528105235-594267",category="lithium")
#' cbk.plot("20130528105235-594267",category="oxygen")
#' cbk.plot("20081202172326.hkitagawa",category="lead")
cbk.plot <- function(stone,category="default") {
  pmlfile <- cbk.download.casteml(c("-r", stone))
  ans <- switch(category,
                "default" = cbk.plot.trace(pmlfile),
                "trace"   = cbk.plot.trace(pmlfile),
                "lithium" = cbk.plot.lithium(pmlfile),
                "oxygen"  = cbk.plot.oxygen(pmlfile),
                "lead"    = cbk.plot.lead(pmlfile),
                stop("No action defined"))
  return(ans)
}
