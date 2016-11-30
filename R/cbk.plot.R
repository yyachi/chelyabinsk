#' @title Plot Analyses with descendants of a stone
#'
#' @description Plot Analyses with descendants of a stone.  This
#'   function downloads CASTEML file associated with a stone and
#'   creates geochemical diagram depending on category specified.
#'
#' @param pmlfile_or_stone A CASTEML file that exits locally or stone-ID (or pmlame)
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
cbk.plot <- function(pmlfile_or_stone,category="default") {
  ans <- -1
  tryCatch({
    ans <- switch(category,
                  "default" = cbk.plot.trace(pmlfile_or_stone),
                  "trace"   = cbk.plot.trace(pmlfile_or_stone),
                  "lithium" = cbk.plot.lithium(pmlfile_or_stone),
                  "oxygen"  = cbk.plot.oxygen(pmlfile_or_stone),
                  "lead"    = cbk.plot.lead(pmlfile_or_stone),
                  stop("No action defined"))
  },error=function(e){
    cbk.plot.message(pmlfile_or_stone,e)
    text(50,60,sprintf("Error in cbk.plot(pmlfile_or_stone,category=\"%s\"):",category),cex=0.8)
  })
  return(ans)
}
