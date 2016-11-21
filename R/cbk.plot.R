#' @title Plot Analyses with descendants of a stone
#'
#' @description Plot Analyses with descendants of a stone.  This
#'   function downloads CASTEML file associated with a stone and
#'   creates geochemical diagram depending on category specified.
#'
#' @param pmlame A dataframe of element abundances (or pmlfile or stone-ID)
#' @param category Category filter that is passed to `casteml convert'
#' @return Dataframe used to plot the diagram
#' @export
#' @seealso \code{\link{cbk.plot.trace}}
#' @examples
#' cbk.plot(cbk.path("20081202172326.hkitagawa.pml"),category="trace")
#' cbk.plot(cbk.path("20081202172326.hkitagawa.pml"),category="lead")
#' cbk.plot(cbk.path("20130528105235-594267.pml"),category="trace")
#' cbk.plot(cbk.path("20130528105235-594267.pml"),category="lithium")
#' cbk.plot(cbk.path("20130528105235-594267.pml"),category="oxygen")
cbk.plot <- function(pmlame,category="default") {
  ans <- switch(category,
                "default" = cbk.plot.trace(pmlame),
                "trace"   = cbk.plot.trace(pmlame),
                "lithium" = cbk.plot.lithium(pmlame),
                "oxygen"  = cbk.plot.oxygen(pmlame),
                "lead"    = cbk.plot.lead(pmlame),
                stop("No action defined"))
  return(ans)
}
