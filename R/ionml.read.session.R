#' @title Read a session stored as a series of IONML
#'
#' @description Read a session stored as a series of IONML.
#'   As of January 5, 2017, this function uses
#'   `ionml.read.laicpqms' to read the IONML.
#' @param ionml.xml Paths to IONML.
#' @param t0 When baseline starts (default: 5 s, scalar or
#'   vector)
#' @param t1 When baseline ends (default: 20 s, scalar or
#'   vector).
#' @param t2 When ion starts (default: 25 s, scalar or vector).
#' @param t3 When ion ends (default: 60 s, scalar or vector).
#' @param ref Reference ion such as `Si29'.
#' @param error Use `sterr', `DL', or `mix' as error.  When `mix',
#'   error is delection limit `DL' instead of standard error `sderr'
#'   when DL is larger than `sderr'.
#' @param intensityp Have mean and sd of intensity instead of mean and
#'   sderr of ionic ratio
#' @param verbose Output debug info (default: FALSE).
#' @param ionml Read IONML file instead of IONCSV (default: TRUE).
#' @return Pseudo-pmlame with average and standard error of ionic
#'   ratio
#' @seealso \code{\link{ionml.read.laicpqms}}
#' @export
#' @examples
#' pmlame0 <- ionml.read.session(cbk.path("ref_cpx_klb1@1.xml"))
#'
#' files <- c(cbk.path("ref_cpx_klb1@1.ion"),cbk.path("ref_cpx_klb1@2.ion"),cbk.path("ref_cpx_klb1@3.ion"))
#' pmlame0 <- ionml.read.session(files,ionml=FALSE)
ionml.read.session <- function(ionml.xml,t0=5,t1=20,t2=25,t3=60,ref="Si29",error="sterr",intensityp=FALSE,verbose=FALSE,ionml=TRUE) {
  if (verbose) {
    cat(file=stderr(),"ionml.read.session:26: t0 # =>",t0,"\n")
    cat(file=stderr(),"ionml.read.session:27: t1 # =>",t1,"\n")
    cat(file=stderr(),"ionml.read.session:28: t2 # =>",t2,"\n")
    cat(file=stderr(),"ionml.read.session:29: t3 # =>",t3,"\n")
  }

  meanlame <- data.frame()
  errorlame <- data.frame()

  t0_ii <- t0; t1_ii <- t1; t2_ii <- t2; t3_ii <- t3 # use same value on schalar

  for(acq_ii in ionml.xml){
    ##* Set individual time when input is vector
    idx <- which(ionml.xml==acq_ii)
    if (length(t0) > 1) {t0_ii <- t0[idx]}
    if (length(t1) > 1) {t1_ii <- t1[idx]}
    if (length(t2) > 1) {t2_ii <- t2[idx]}
    if (length(t3) > 1) {t3_ii <- t3[idx]}

    ##* Load data
    acqName                    <- tools::file_path_sans_ext(basename(acq_ii))
    ## ionlame0                <- ionml.read.laicpqms(acq_ii,ref=ref)
    ionlame0                   <- ionml.read.laicpqms(acq_ii,t0=t0_ii,t1=t1_ii,t2=t2_ii,t3=t3_ii,ref=ref,verbose=verbose,ionml=ionml)
    ionlame                    <- ionlame0[,colnames(ionlame0)!="time"] # drop column of `time'

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
    } else if (error=="mix") { # Replace sderr by detection limit, if larger
      acqDL                    <- ionlame[paste0("DL/",ref),]
      idx_overwrite            <- acqDL > acqError
      acqError[,idx_overwrite] <- acqDL[,idx_overwrite] # DL instead of sderr
    } else if (error=="DL") {  # Replace sderr by detection limit, by any mean
      acqDL                    <- ionlame[paste0("DL/",ref),]
      acqError                 <- acqDL
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
