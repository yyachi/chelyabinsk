#' @title Return isotope information
#'
#' @description Return isotope information.  It is in a form of pmlame.
#'
#' @return A pmlame of isotope information
#' @seealso \code{\link{cbk.periodic}}
#' @export
#' @examples
#' cbk.iso()['Li7','ratio']
#' cbk.iso()['Li7','weight']
cbk.iso <- function(){
  isolame <- cbk.read.tblame(cbk.path("isotope-tblame0.csv"))
  return(isolame)
}
