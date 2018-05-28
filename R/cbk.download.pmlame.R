#' @title Download analysis records from Medusa and return a pmlame
#'
#' @description Download analysis records from Medusa and return a
#'   pmlame.  The pmlame is stored in a temporary directory as a
#'   TBLAME.csv unless specified.  Note with the same arguments, this
#'   function downloads only once per an R session.
#'
#' @param stone Unique identification number of stones in Medusa.
#' @param file File path to save downloaded TBLAME.csv.  Default is
#'   a temporary file in a temporary directory.
#' @param tableunit Output unit that will be resolved by
#'   cbk.convector() (default: "none").
#' @param force Force download a pmlame (default: FALSE).
#' @return A pmlame with unit organized.
#' @export
#' @seealso \code{\link{cbk.download.casteml}}, and
#'   \url{https://github.com/misasa/casteml}
#' @examples
#' pmlame <- cbk.download.pmlame("20081202172326.hkitagawa")
cbk.download.pmlame <- function(stone,opts=NULL,file=NULL,tableunit="none",force=FALSE,verbose=TRUE) {
  library(MedusaRClient)
  opts_default <- list(Recursivep=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)

  if (verbose) {
    cat(file=stderr(),"cbk.download.pmlame:25: stone # =>",stone,"\n")
  }

  if(is.null(file)){
    file <- file.path(tempdir(),paste0(digest::digest(stone,algo='md5'),".csv"))
  }

  ## Download file only when it does not exist
  if (force || !file.exists(file)) {
    ## cat(file=stderr(),"cbk.download.pmlame:25: cmd # =>",stone,"\n")
    pmlames <- list()
    for(ii in 1:length(stone)) {
      if(opts$Recursivep){
        pmlame0 <- medusaRClient.read.pmlame(stone[ii],opts=list(Recursivep=TRUE))
      } else {
        pmlame0 <- medusaRClient.read.pmlame(stone[ii])
      }
      pmlames[[ii]] <- pmlame0
    }
    pmlame <- do.call(cbk.lame.merge,pmlames)
    cbk.write.tblame(pmlame,file)
  } else {
    pmlame <- cbk.read.tblame(file)
  }
  
  if(tableunit!='none'){
    pmlame0 <- pmlame[,grepl("^[A-Z].*",colnames(pmlame))]  # extract elements
    pmlame1 <- pmlame0 * cbk.convector(tableunit)
    pmlame  <- cbind(pmlame[,!grepl("^[A-Z].*",colnames(pmlame))],pmlame1)
  }

  chemlist                    <- colnames(pmlame)
  property0                   <- cbk.periodic("atomicnumber")[chemlist] # atomicnumber, volatility, compatibility
  names(property0)            <- chemlist
  property0[is.na(property0)] <- 999
  property1                   <- sort(property0)
  pmlame                      <- pmlame[,names(property1),drop=FALSE]

  errorp                      <- grepl("_error",colnames(pmlame))
  if (any(errorp)) {
    chemlame  <- cbk.lame.regulate(pmlame,error=FALSE)
    extralame <- cbk.lame.regulate(pmlame,mean=FALSE,error=FALSE,extra=TRUE)
    errorlame <- cbk.lame.fetch.error(pmlame)
    pmlame    <- cbk.lame.merge.error(chemlame,errorlame)
    pmlame    <- cbind(pmlame, extralame)
  }
  return(pmlame)
}
