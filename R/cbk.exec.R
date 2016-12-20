#' @title Open a file or URL using Windows/Macintosoh file associations
#'
#' @description Open the specified file or URL using the application
#'   specified in the Windows/Macintosh file associations.  On Windows
#'   and Macintosh, this calls \code{\link{shell.exec}} and
#'   \code{\link{system}}, respectively.
#'
#' @details The path in \code{file} is interpreted relative to the
#'   current working directory.
#' @seealso \code{\link{shell.exec}} and \code{\link{system}}
#' @param file file or URL to be opened
#' @return result by opening the file
#' @export
#' @examples
#' cbk.exec(".")
cbk.exec <- function(file){
  # Idea is from
  # http://stackoverflow.com/questions/12273346/system-independent-method-of-opening-afile

  # replacement for shell.exe (doesn't exist on MAC)
  if (exists("shell.exec",where = "package:base"))
    return(base::shell.exec(file))
  return(system(paste("open",file)))
}
