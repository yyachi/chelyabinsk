#' @title Read a session stored as a series of ion-type TBLAME.csv
#'
#' @description Read a session stored as a series of ion-type
#'   TBLAME.CSV.  As of January 5, 2017, this function uses
#'   `ionml.read.laicpqms' to read the ion-type TBLAME.CSV.
#' @param tblame.csv paths to ion-type TBLAME.CSV
#' @param ref reference ion such as `Si29'
#' @return pseudo-pmlame with average and standard error of ionic ratio
#' @seealso \code{\link{ionml.read.laicpqms}}
#' @export
#' @examples
#' files <- c(cbk.path("ref_cpx_klb1@1.ion"),cbk.path("ref_cpx_klb1@2.ion"),cbk.path("ref_cpx_klb1@3.ion"))
#' pmlame0 <- ionml.read.session(files)
ionml.read.session <- function(tblame.csv,ref="Si29") {
  pmMean0               <- data.frame() # summary
  pmSderr0              <- data.frame() # summary

  for(acq_ii in tblame.csv){
    acqname             <- tools::file_path_sans_ext(basename(acq_ii))
    pmStat1             <- ionml.read.laicpqms(acq_ii,ref=ref)
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
