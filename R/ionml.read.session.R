#' @title Read a session stored as a series of icnml-type TBLAME.csv
#'
#' @description Read a session stored as a series of icnml-type
#'   TBLAME.csv.  As of January 5, 2017, this function uses
#'   `ionml.read.laicpqms' to read the icnml-type TBLAME.csv.
#' @param tblame.csv Paths to icnml-type TBLAME.csv
#' @param ref Reference ion such as `Si29'
#' @return The icnml-type pmlame with average and standard error
#' @seealso \code{\link{ionml.read.laicpqms}}
#' @export
ionml.read.session <- function(tblame.csv,ref="Si29") {
  pmMean0               <- data.frame()
  pmSderr0              <- data.frame()

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
