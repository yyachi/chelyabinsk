#' @title Plot Analyses with descendants of a stone
#'
#' @description Plot Analyses with descendants of a stone.  This
#'   function downloads CASTEML file associated with a stone and
#'   creates geochemical diagram depending on category specified.
#'
#' @param pmlfile_or_stone A CASTEML file that exits locally or
#'   stone-ID (or pmlame).
#' @param category Category specifier that is passed to \code{casteml
#'   convert}.
#' @param opts List of further options for plot.  Two options such as
#'   `list(legendp=TRUE, Recursivep=FALSE)' are available.
#' @return A pmlame used to plot the diagram.
#' @export
#' @seealso \code{\link{cbk.plot.trace}}
#' @examples
#' cbk.plot(c("20141106120733-593074","20141106120433-380274","20141106120509-983968"))
#'
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' message(sprintf("The pmlfile is located at |%s|.",pmlfile))
#' cbk.plot(pmlfile)
#' cbk.plot(pmlfile,category="trace")
#' cbk.plot(pmlfile,category="lead")
#' cbk.plot(pmlfile,opts=list(legendp=FALSE))
#'
#' pmlfile <- cbk.path("20130528105235-594267.pml")
#' message(sprintf("The pmlfile is located at |%s|.",pmlfile))
#' cbk.plot(pmlfile,category="trace")
#' cbk.plot(pmlfile,category="lithium")
#' cbk.plot(pmlfile,category="oxygen")
cbk.plot <- function(pmlfile_or_stone,opts=NULL,category=NULL,...) {
  opts_default <- list(legendp=TRUE, Recursivep=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared option
  opts <- c(opts,opts_default)

  ans <- -1
  tryCatch({
    ## if (category == "default") {
    if (is.null(category)) {
      category <- cbk.category(pmlfile_or_stone)[1]
    }

    ans <- switch(category,
                  ## "default" = cbk.plot.trace(pmlfile_or_stone),
                  "trace"   = cbk.plot.trace(pmlfile_or_stone,opts,...),
                  "spider"  = cbk.plot.spider(pmlfile_or_stone,opts,...),
                  "REE"     = cbk.plot.ree(pmlfile_or_stone,opts,...),
                  "lithium" = cbk.plot.lithium(pmlfile_or_stone,opts,...),
                  "oxygen"  = cbk.plot.oxygen(pmlfile_or_stone,opts,...),
                  "lead"    = cbk.plot.lead(pmlfile_or_stone,opts,...),
                  "spots"   = cbk.plot.spots(pmlfile_or_stone,opts,...),
                  stop("No action defined"))
  },error=function(msg){
    cbk.plot.message(pmlfile_or_stone,sQuote(msg))
    ## text(50,60,sprintf("Error in cbk.plot(pmlfile_or_stone,category=\"%s\"):",category),cex=0.8)
    ## text(0,50,msg,cex=0.8,adj=c(0,NA))
  })
  return(ans)
}
