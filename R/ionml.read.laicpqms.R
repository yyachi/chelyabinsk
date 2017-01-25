#' @title Read ion-type TBLAME.CSV originated from Analyte G2 with
#'   iCAP-Q
#'
#' @description Read ion-type TBLAME.CSV originated from Analyte G2
#'   with iCAP-Q.  The original QTEGRA.CSV (CSV file exported from
#'   Qtegra) should be processed in advance to have ion-type
#'   TBLNAME.CSV with extension `.ion'.
#'
#'   The ion-type TBLNAME.CSV consists of columns of time and ion
#'   intensities.  The first column of each line should be number of
#'   `cycle'.  Colname of the ion-type TBLNAME.CSV should be `time'
#'   and name of element followed by atomic weight (`Si29' instead of
#'   `29Si').
#' @param pmlame_or_file ion-type pmlame or ion-type TBLAME.CSV with
#'   extension `.ion'
#' @param t0 time when baseline starts (default: 5 s)
#' @param t1 time when baseline ends (default: 20 s)
#' @param t2 time when ion starts (default: 25 s)
#' @param t3 time when ion ends (default: 60 s)
#' @param ref reference ion such as "Si29"
#' @return The ion-type pmlame of ion/ref online with rows of
#'   statistical information
#' @export
#' @seealso \code{\link{ionml.convert.laicpqms}}
#' @examples
#' file <- cbk.path("ref_cpx_klb1@1.ion")
#' pmlfile0 <- ionml.read.laicpqms(file)
ionml.read.laicpqms <- function(pmlame_or_file,t0=5,t1=20,t2=25,t3=60,ref="Si29") {
  library(dplyr)

  ## Setup I/O
  if (is.data.frame(pmlame_or_file)) { # pmlame fed
    pmlame0           <- pmlame_or_file
  } else { # file fed
    ionbase           <- tools::file_path_sans_ext(pmlame_or_file)

    ## Force set extension of ionfile
    ionfile <- paste0(ionbase,".ion")

    ## automatic conversion on miss of ionfile
    if (!file.exists(ionfile)) {
      ionfile <- ionml.convert.laicpqms(ionbase)
    }

    ## load from ionfile
    pmlame0           <- cbk.read.tblame(ionfile)
  }

  ## Stat baseline
  pmlame1             <- filter(pmlame0, time >t0 & time <t1) # baseline
  pmMean1             <- summarise_each(pmlame1, funs(mean))
  pmMean1$time        <- NA
  row.names(pmMean1)  <- "base/cps"

  ## Subtract
  pmlame2             <- sweep(pmlame0, 2, as.numeric(pmMean1)) # baseline subtracted from raw signal
  pmlame2$time        <- pmlame0$time

  ## Stat ion online
  pmlame3             <- filter(pmlame2, time >t2 & time <t3) # ion online
  pmMean3             <- summarize_each(pmlame3, funs(mean))
  row.names(pmMean3)  <- "mean/cps"

  ## Normalize
  pmlame4             <- pmlame2 / pmlame2[,ref] # ion normaized by ref
  pmlame4$time        <- pmlame0$time
  pmlame5             <- filter(pmlame4, time >t2 & time <t3) # ion/ref online

  ## Stat ion/ref online
  pmMean5             <- summarize_each(pmlame5, funs(mean))
  row.names(pmMean5)  <- paste0("mean/",ref)
  pmStdev5            <- summarize_each(pmlame5, funs(sd))
  row.names(pmStdev5) <- paste0("stdev/",ref)
  pmSderr5            <- summarize_each(pmlame5, funs(sd)) / sqrt(nrow(pmlame5))
  row.names(pmSderr5) <- paste0("sderr/",ref)
  pmSrel5             <- pmSderr5 / pmMean5 * 100
  row.names(pmSrel5)  <- "sderr%"

  ## Summarize stats
  pmStat5             <- rbind(pmMean1,pmMean3,pmMean5,pmStdev5,pmSderr5,pmSrel5)
  pmStat5$time        <- NA

  ## Make up for output
  ## pmStat6          <- pmStat5[,colnames(pmStat5)!="time"] # drop column of `time'
  pmlame6             <- rbind(pmlame5,pmStat5) # Merge ion/ref and stat

  return(pmlame6)
}
