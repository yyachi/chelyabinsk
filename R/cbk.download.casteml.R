#' @title Download analysis records as a CASTEML file
#'
#' @description Download analysis records as a CASTEML file.  This
#'   function returns path to the file.  The file is stored in a
#'   temporary directory unless specified.  Note with the same
#'   arguments, this function downloads only once per an R session.
#'
#' @details Revision on 2017-04-11 changed interface of the first
#'   argument `stone'.  It used be an array consists of stones and
#'   option to pass to `casteml download'.  Now this function
#'   downloads CASTEML file by its own and the argument `stone' only
#'   accepts a stone.  An user should specify Recursive and recursive
#'   option not a part of argument `stone' but explicitly.
#' 
#' @param stone Unique identification number of a stone in Medusa.
#' @param file File path to save downloaded CASTEML file.  Default is
#'   a temporary file in a temporary directory.
#' @param force Force download CASTEML file.
#' @param directDownload Download CASTEML file via HTTP otherwise via
#'   `casteml download'.
#' @param directAuth Parameter for HTTP basic authentication.
#' @param Recursive Download a whole family of a stone (equivalent to
#'   --family in casteml download)
#' @param recursive Download descendants of a stone (equivalent to
#'   --descendant in casteml download).
#' @return Path to CASTEML file that was downloaded.
#' @export
#' @seealso \code{casteml download},
#'   \url{https://github.com/misasa/casteml}, and
#'   \code{\link{cbk.convert.casteml}}
#' @examples
#' stone <- c("20080616170000.hk","20080616170056.hk","20080616170054.hk")
#' pmlfiles <- lapply(stone, cbk.download.casteml)
#'
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' directAuth <- list(uri="dream.misasa.okayama-u.ac.jp/demo/",user='admin',password='admin')
#' pmlfile <- cbk.download.casteml("20110416134901-075-241",directAuth=directAuth)
#' pmlfile <- cbk.download.casteml("20110416134901-075-241",directAuth=directAuth,recursive=TRUE)
#' pmlfile <- cbk.download.casteml("20110416134901-075-241",directAuth=directAuth,Recursive=TRUE)
cbk.download.casteml <- function(stone,file=NULL,force=FALSE,directDownload = TRUE,directAuth=NULL,Recursive=FALSE,recursive=FALSE ) {
  ## cat(file=stderr(),"cbk.download.casteml: stone is |",stone,"|\n")
  cat(file=stderr(),"cbk.download.casteml:26: stone # =>",stone,"\n")

  if(directDownload){
    pmlfiles <- c()
    for(ii in 1:length(stone)) {
      pmlfiles <- append(pmlfiles,cbk.download.casteml.1(stone[ii],force=force,directAuth=directAuth,Recursive=Recursive,recursive=recursive))
    }
    join_cmd <- paste(c("casteml join",pmlfiles),collapse=" ")
    seed     <- join_cmd
  } else {
    cmd <- paste(c("casteml download",stone),collapse=" ")
    if(Recursive){
      cmd <- paste(c("casteml download",c('-R', stone)),collapse=" ")
    } else if(recursive){
      cmd <- paste(c("casteml download",c('-r', stone)),collapse=" ")
    }
    seed <- cmd
  }

  ## file <- tempfile(pattern = paste(stone[1],"@",sep=""), fileext=".pml")
  ## system(paste("casteml download",stone[ii],">",file))

  if(is.null(file)){
    ## file <- tempfile(fileext=".pml")
    file <- file.path(tempdir(),paste0(digest::digest(seed,algo='md5'),".pml"))
  }

  ## Download file only when it does not exist
  if (force || !file.exists(file)) {
    cat(file=stderr(),"cbk.download.casteml:81: cmd # =>",seed,"\n")
    cat(system(seed, intern = TRUE),file=file,sep="\n")
  }
  return(file)
}
