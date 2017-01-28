#' @title Read a session stored as a series of ion-type TBLAME.csv
#'
#' @description Read a session stored as a series of ion-type
#'   TBLAME.csv.  As of January 5, 2017, this function uses
#'   `ionml.read.laicpqms' to read the ion-type TBLAME.csv.
#' @param tblame.csv Paths to ion-type TBLAME.csv.
#' @param t0 Time when baseline starts (default: 5 s).
#' @param t1 Time when baseline ends (default: 20 s).
#' @param t2 Time when ion starts (default: 25 s).
#' @param t3 Time when ion ends (default: 60 s).
#' @param ref Reference ion such as `Si29'.
#' @param DL Use delection limit (DL) instead of `sderr' when DL is
#'   larger than `sderr'.
#' @param intensityp Have mean and sd of intensity instead of mean and
#'   sderr of ionic ratio
#' @return Pseudo-pmlame with average and standard error of ionic
#'   ratio
#' @seealso \code{\link{ionml.read.laicpqms}}
#' @export
#' @examples
#' files <- c(cbk.path("ref_cpx_klb1@1.ion"),cbk.path("ref_cpx_klb1@2.ion"),cbk.path("ref_cpx_klb1@3.ion"))
#' pmlame0 <- ionml.read.session(files)
ionml.read.session <- function(tblame.csv,t0=5,t1=20,t2=25,t3=60,ref="Si29",DL=FALSE,intensityp=FALSE) {
  ## cat(file=stderr(),"ionml.read.session:22: t0 # =>",t0,"\n")
  ## cat(file=stderr(),"ionml.read.session:23: t1 # =>",t1,"\n")
  ## cat(file=stderr(),"ionml.read.session:24: t2 # =>",t2,"\n")
  ## cat(file=stderr(),"ionml.read.session:25: t3 # =>",t3,"\n")

  meanlame                     <- data.frame()
  errorlame                    <- data.frame()

  for(acq_ii in tblame.csv){
    ##* Load data
    acqName                    <- tools::file_path_sans_ext(basename(acq_ii))
    ## ionlame0                <- ionml.read.laicpqms(acq_ii,ref=ref)
    ionlame0                   <- ionml.read.laicpqms(acq_ii,t0=t0,t1=t1,t2=t2,t3=t3,ref=ref)
    ionlame                   <- ionlame0[,colnames(ionlame0)!="time"] # drop column of `time'

    ##* Deal mean
    acqMean                    <- ionlame[paste0("mean/",ref),] # normalized intensity
    if (intensityp) {
      acqMean                  <- ionlame["mean/cps",] # raw intensity
    }
    row.names(acqMean)         <- acqName
    meanlame                   <- rbind(meanlame, acqMean)

    ##* Deal error
    acqError                   <- ionlame[paste0("sderr/",ref),] # sderr
    if (intensityp) {
      acqError                 <- ionlame["sd/cps",]
    } else if (DL) { # Replace sderr by detection limit, if larger
      acqDL                    <- ionlame[paste0("DL/",ref),]
      idx_overwrite            <- acqDL > acqError
      acqError[,idx_overwrite] <- acqDL[,idx_overwrite] # DL instead of sderr
    }
    row.names(acqError)        <- acqName
    errorlame                  <- rbind(errorlame, acqError)
  }

  ##* Rename column of error to be similar to CASTEML
  colnames(errorlame) <-paste0(colnames(errorlame),"_error")

  ##* Merge columns of mean and standard-error interleavely
  fullame             <- cbind(meanlame,errorlame)
  n                   <- ncol(meanlame)
  index               <- rep(1:n, each = 2) + (0:1) * n
  fullame1            <- fullame[index]

  return(fullame1)
}
