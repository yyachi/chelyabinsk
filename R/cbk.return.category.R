#' Return list of categories that are acceptable by `cbk.plot'
#' @return A list of categories that are acceptable by `cbk.plot'
#' @export
#' @examples
#' categorylist <- cbk.return.category()
cbk.return.category <- function(){
  categorylist = list(
    "trace",
    "lithium",
    "oxygen",
    "lead")
  return(categorylist)
}
