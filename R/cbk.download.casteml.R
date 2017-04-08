#' @title Download analysis records as a CASTEML file
#'
#' @description Download analysis records as a CASTEML file.  This
#'   function returns path to the file.  The file is stored in a
#'   temporary directory unless specified.  Note with the same
#'   arguments, this function downloads only once per a R session.
#' 
#' @param stone Unique indentification number of stones in Medusa.
#'   Really, those will pass to `casteml download' and thus you can
#'   include options.
#' @param file File path to save downloaded CASTEML file
#' @param force Force download CASTEML file
#' @param useHTTP Force download CASTEML file via HTTP
#' @param paramHTTP Parameter for HTTP basic authentication
#' @param Recursive With -R option
#' @param recursive With -r option
#' @return Path to CASTEML file that was downloaded in temporary
#'   directory.
#' @export
#' @seealso \code{casteml download},
#'   \url{https://github.com/misasa/casteml}, and
#'   \code{\link{cbk.convert.casteml}}
#' @examples
#' stone <- c("20080616170000.hk","20080616170056.hk","20080616170054.hk")
#' pmlfiles <- lapply(stone, cbk.download.casteml)
#'
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' paramHTTP <- list(uri="dream.misasa.okayama-u.ac.jp/demo/",user='admin',password='admin')
#' pmlfile <- cbk.download.casteml("20110416134901-075-241",paramHTTP=paramHTTP)
#' pmlfile <- cbk.download.casteml("20110416134901-075-241",paramHTTP=paramHTTP,recursive=TRUE)
#' pmlfile <- cbk.download.casteml("20110416134901-075-241",paramHTTP=paramHTTP,Recursive=TRUE)
cbk.download.casteml <- function(stone,file=NULL,force=FALSE,useHTTP = TRUE,paramHTTP=NULL,Recursive=FALSE,recursive=FALSE ) {
  ## cat(file=stderr(),"cbk.download.casteml: stone is |",stone,"|\n")
  cat(file=stderr(),"cbk.download.casteml:26: stone # =>",stone,"\n")

  if(useHTTP){
    library(httr)
    if(is.null(paramHTTP)){
      library(yaml)
      orochirc <- yaml.load_file("~/.orochirc")
      url <- paste("http://",orochirc$uri, sep="")
      user <- orochirc$user
      password <- orochirc$password
    } else {
      url <- paste("http://",paramHTTP$uri, sep="")
      user <- paramHTTP$user
      password <- paramHTTP$password      
    }
    my_url <- paste(url,"records/",stone,".pml",sep="")
    if(Recursive){
      my_url <- paste(url,"records/",stone,"/families.pml",sep="")
    } else if(recursive){
      my_url <- paste(url,"records/",stone,"/self_and_descendants.pml",sep="")
    }
    seed <- my_url
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
    if(useHTTP){
      cat(file=stderr(),"cbk.download.casteml:76: url # =>",my_url,"\n")
      req <- GET(my_url,authenticate(user,password,type="basic"))
      bin <- content(req, "raw")
      writeBin(bin, file)
    } else {
      cat(file=stderr(),"cbk.download.casteml:81: cmd # =>",cmd,"\n")
      cat(system(cmd, intern = TRUE),file=file,sep="\n")
    }
  }

  return(file)
}
