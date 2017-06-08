#' @title Download analysis records as a CASTEML file via HTTP
#'
#' @description Download analysis records as a CASTEML file via HTTP.
#'   This function returns path to the file.  The file is stored in a
#'   temporary directory unless specified.  Note with the same
#'   arguments, this function downloads only once per an R session.
#'
#' @details This function was created on 2017-04-11 to download
#'   CASTEML file by its own from Medusa and the first argument can
#'   only accept single `stone' unlike \link{cbk.download.casteml} can
#'   accept an array consists of stones.
#'
#' @param stone Unique identification number of a stone in Medusa.
#' @param file File path to save downloaded CASTEML file.  Default is
#'   a temporary file in a temporary directory.
#' @param force Force download CASTEML file.
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
#'   \code{\link{cbk.download.casteml}}
#' @examples
#' stone <- c("20080616170000.hk","20080616170056.hk","20080616170054.hk")
#' pmlfiles <- lapply(stone, cbk.download.casteml.1)
#'
#' pmlfile <- cbk.download.casteml.1("20081202172326.hkitagawa")
#' directAuth <- list(uri="dream.misasa.okayama-u.ac.jp/demo/",user='admin',password='admin')
#' pmlfile <- cbk.download.casteml.1("20110416134901-075-241",directAuth=directAuth)
#' pmlfile <- cbk.download.casteml.1("20110416134901-075-241",directAuth=directAuth,recursive=TRUE)
#' pmlfile <- cbk.download.casteml.1("20110416134901-075-241",directAuth=directAuth,Recursive=TRUE)
cbk.download.casteml.1 <- function(stone,file=NULL,force=FALSE,directAuth=NULL,Recursive=FALSE,recursive=FALSE ) {
  ## cat(file=stderr(),"cbk.download.casteml.1: stone is |",stone,"|\n")
  cat(file=stderr(),"cbk.download.casteml.1:26: stone # =>",stone,"\n")

  ## if(directDownload){
  library(httr)
  if(is.null(directAuth)){
    library(yaml)
    orochirc <- yaml.load_file("~/.orochirc")
    url <- paste0("http://",orochirc$uri)
    user <- orochirc$user
    password <- orochirc$password
  } else {
    url <- paste0("http://",directAuth$uri)
    user <- directAuth$user
    password <- directAuth$password
  }
  url <- gsub("/$","",url) # take out slash
  ## cat(file=stderr(),"cbk.download.casteml.1:50: url <-",cbk.lame.dump(url,show=F),"\n")
  my_url <- paste0(url,"/records/",stone,".pml")
  if(Recursive){
    my_url <- paste0(url,"/records/",stone,"/families.pml")
  } else if(recursive){
    my_url <- paste0(url,"/records/",stone,"/self_and_descendants.pml")
  }
  seed <- my_url
  ## } else {
  ##   cmd <- paste(c("casteml download",stone),collapse=" ")
  ##   if(Recursive){
  ##     cmd <- paste(c("casteml download",c('-R', stone)),collapse=" ")
  ##   } else if(recursive){
  ##     cmd <- paste(c("casteml download",c('-r', stone)),collapse=" ")
  ##   }
  ##   seed <- cmd
  ## }

  ## file <- tempfile(pattern = paste(stone[1],"@",sep=""), fileext=".pml")
  ## system(paste("casteml download",stone[ii],">",file))
  if(is.null(file)){
    ## file <- tempfile(fileext=".pml")
    file <- file.path(tempdir(),paste0(digest::digest(seed,algo='md5'),".pml"))
  }

  ## Download file only when it does not exist
  if (force || !file.exists(file)) {
    ## if(directDownload){
      cat(file=stderr(),"cbk.download.casteml.1:83: url # =>",my_url,"\n")
      req <- GET(my_url,authenticate(user,password,type="basic"))
      bin <- content(req, "raw")
      writeBin(bin, file)
    ## } else {
    ##   cat(file=stderr(),"cbk.download.casteml.1:88: cmd # =>",cmd,"\n")
    ##   cat(system(cmd, intern = TRUE),file=file,sep="\n")
    ## }
  }

  return(file)
}
