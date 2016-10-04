#' Open file even on Mac
#' @param file A filename
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
  comm <- paste("open",file)
  return(system(comm))
}
