#' Open file even on Mac.  This is copy from
#' http://stackoverflow.com/questions/12273346/system-independent-method-of-opening-afile
#' @param x A filename
#' @return result
#' @export
cas.exec <- function(x){
  # replacement for shell.exe (doesn't exist on MAC)
  if (exists("shell.exec",where = "package:base"))
    return(base::shell.exec(x))
  comm <- paste("open",x)
  return(system(comm))
}
