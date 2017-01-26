#' @title Read a session stored as a series of ion-type TBLAME.csv
#'
#' @description Read a session stored as a series of ion-type
#'   TBLAME.csv.  As of January 5, 2017, this function uses
#'   `ionml.read.laicpqms' to read the ion-type TBLAME.csv.
#' @param tblame.csv paths to ion-type TBLAME.csv
#' @param t0 time when baseline starts (default: 5 s)
#' @param t1 time when baseline ends (default: 20 s)
#' @param t2 time when ion starts (default: 25 s)
#' @param t3 time when ion ends (default: 60 s)
#' @param ref reference ion such as "Si29"
#' @return pseudo-pmlame with average and standard error of ionic ratio
#' @seealso \code{\link{ionml.read.laicpqms}}
#' @export
#' @examples
#' files <- c(cbk.path("ref_cpx_klb1@1.ion"),cbk.path("ref_cpx_klb1@2.ion"),cbk.path("ref_cpx_klb1@3.ion"))
#' pmlame0 <- ionml.read.session(files)
ionml.read.session <- function(tblame.csv,t0=5,t1=20,t2=25,t3=60,ref="Si29") {
  pmMean0               <- data.frame() # summary
  pmSderr0              <- data.frame() # summary

  ## cat(file=stderr(),"ionml.read.session:22: t0 # =>",t0,"\n")
  ## cat(file=stderr(),"ionml.read.session:23: t1 # =>",t1,"\n")
  ## cat(file=stderr(),"ionml.read.session:24: t2 # =>",t2,"\n")
  ## cat(file=stderr(),"ionml.read.session:25: t3 # =>",t3,"\n")

  for(acq_ii in tblame.csv){
    acqname             <- tools::file_path_sans_ext(basename(acq_ii))
    ## pmStat1             <- ionml.read.laicpqms(acq_ii,ref=ref)
    pmStat1             <- ionml.read.laicpqms(acq_ii,t0=t0,t1=t1,t2=t2,t3=t3,ref=ref)
    pmStat2             <- pmStat1[,colnames(pmStat1)!="time"] # drop column of `time'

    pmMean2             <- pmStat2[paste0("mean/",ref),]
    row.names(pmMean2)  <- acqname
    pmMean0             <- rbind(pmMean0, pmMean2)

    pmSderr2            <- pmStat2[paste0("sderr/",ref),]
    row.names(pmSderr2) <- acqname
    pmSderr0            <- rbind(pmSderr0, pmSderr2)
  }

  # Rename column of standard-error to be similar to CASTEML
  colnames(pmSderr0)    <-paste0(colnames(pmSderr0),"_error")

  # Merge columns of mean and standard-error interleavely
  pmStat1               <- cbind(pmMean0,pmSderr0)
  n                     <- ncol(pmMean0)
  index                 <- rep(1:n, each = 2) + (0:1) * n
  pmStat2               <- pmStat1[index]

  return(pmStat2)
}
