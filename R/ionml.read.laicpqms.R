#' @title Read ion-type TBLAME.csv originated from Analyte G2 with
#'   iCAP-Q
#'
#' @description Read ion-type TBLAME.csv originated from Analyte G2
#'   with iCAP-Q.  The original QTEGRA.csv (CSV file exported from
#'   Qtegra) should be processed in advance to be ion-type
#'   TBLNAME.csv with extension `.ion'.
#'
#'   The ion-type TBLNAME.csv consists of columns of time and ion
#'   intensities.  The first column of each line should be number of
#'   `cycle'.  Colname of the ion-type TBLNAME.csv should be `time'
#'   and name of element followed by atomic weight (`Si29' instead of
#'   `29Si').
#'
#' @details Signal between `t2' and `t3' is regarded as main signal
#'   from a sample (online).  Signal between `t0' and `t1' is regarded
#'   as background.  Mean of latter is calculated as BASELINE.  Then
#'   the BASELINE is subtracted from the main signal.  The main signal
#'   is normalized by `ref'.  This function returns the BASELINE
#'   subtracted and reference normalized `ionic-ratio' with
#'   statistical information at the bottom of the table.  Detection
#'   limit is defined by 3 times standard error of BASELINE measument.
#'
#' @param pmlame_or_file ion-type pmlame or ion-type TBLAME.csv with
#'   extension `.ion'.
#' @param t0 When baseline starts (default: 5 s).
#' @param t1 When baseline ends (default: 20 s).
#' @param t2 When ion starts (default: 25 s).
#' @param t3 When ion ends (default: 60 s).
#' @param ref reference ion such as `Si29'.
#' @param verbose Output debug info (default: FALSE).
#' @return The ion-type pmlame of ion-to-ref ratio online with rows of
#'   statistical information.
#' @export
#' @seealso \code{\link{ionml.convert.laicpqms}}
#' @examples
#' file <- cbk.path("ref_cpx_klb1@1.ion")
#' pmlfile0 <- ionml.read.laicpqms(file)
ionml.read.laicpqms <- function(pmlame_or_file,t0=5,t1=20,t2=25,t3=60,ref="Si29",verbose=FALSE) {
  library(dplyr)

  if (verbose) {
    cat(file=stderr(),"ionml.read.laicpqms:42: t0 t1 t2 t3 # =>",t0,t1,t2,t3,"\n")
  }

  ## Setup I/O
  if (is.data.frame(pmlame_or_file)) { # pmlame fed
    pmlame0           <- pmlame_or_file
  } else { # file fed
    ionbase           <- tools::file_path_sans_ext(pmlame_or_file)
    fileext           <- tools::file_ext(pmlame_or_file)

    if (fileext == "xml") {
      xmlfile <- pmlame_or_file
      if (!file.exists(xmlfile)) {
        iontblame <- ionml.convert.laicpqms(ionbase,outfile=tempfile(fileext=".ion"))
        xmlfile   <- ionml.convert.iontblame(iontblame,outfile=tempfile(fileext=".xml"))
      }
      pmlame0     <- cbk.read.ionml(xmlfile)
    } else {
      ## Force set extension of ionfile
      ionfile <- paste0(ionbase,".ion")

      ## automatic conversion on miss of ionfile
      if (!file.exists(ionfile)) {
        ionfile <- ionml.convert.laicpqms(ionbase)
      }

      ## load from ionfile
      pmlame0           <- cbk.read.tblame(ionfile)
    }
  }

  ## Stat baseline
  pmlame1             <- filter(pmlame0, time >t0 & time <t1) # baseline
  n_baseline          <- nrow(pmlame1)
  if (verbose) {
    cat(file=stderr(),"ionml.read.laicpqms:68: n_baseline <-",cbk.lame.dump(n_baseline,show=F),"\n")
  }

  pmMean1             <- summarise_each(pmlame1, funs(mean))
  pmMean1$time        <- NA
  row.names(pmMean1)  <- "mean(base)/cps"

  ## Subtract baseline from main signal
  pmlame2             <- sweep(pmlame0, 2, as.numeric(pmMean1)) # baseline subtracted from raw signal
  pmlame2$time        <- pmlame0$time

  ## Stat main signal online
  pmlame3             <- filter(pmlame2, time >t2 & time <t3) # ion online
  n_online            <- nrow(pmlame3)

  pmMean3             <- summarize_each(pmlame3, funs(mean))
  row.names(pmMean3)  <- "mean/cps"

  pmSd3               <- summarize_each(pmlame3, funs(sd))
  row.names(pmSd3)    <- "sd/cps"

  ## Detection limit defined as 3 x Sd(base) / ref / sqrt(n) during background period
  pmSd1               <- summarise_each(pmlame1, funs(sd))
  pmSd1$time          <- NA
  row.names(pmSd1)    <- "sd(base)/cps"

  refOverallIntMean   <- pmMean3["mean/cps",ref] # one number for one DL
  pmDL3               <- pmSd1*3/ refOverallIntMean / sqrt(n_baseline)
  row.names(pmDL3)    <- paste0("DL/",ref)
  if (verbose) {
    cat(file=stderr(),"ionml.read.laicpqms:99: refOverallIntMean <-",cbk.lame.dump(refOverallIntMean,show=F),"\n")
  }

  ## Normalize
  pmlame4             <- pmlame2 / pmlame2[,ref] # ion normaized by ref
  pmlame4$time        <- pmlame0$time
  pmlame5             <- filter(pmlame4, time >t2 & time <t3) # ion/ref online

  ## Stat ion/ref online
  pmMean5             <- summarize_each(pmlame5, funs(mean))
  row.names(pmMean5)  <- paste0("mean/",ref)
  pmSd5               <- summarize_each(pmlame5, funs(sd))
  row.names(pmSd5)    <- paste0("sd/",ref)
  pmSderr5            <- summarize_each(pmlame5, funs(sd)) / sqrt(nrow(pmlame5))
  row.names(pmSderr5) <- paste0("sderr/",ref)
  Sdrel5              <- pmSderr5 / pmMean5 * 100
  row.names(Sdrel5)   <- "sderr%"

  ## Summarize stats
  pmStat5             <- rbind(pmMean1,pmSd1,pmMean3,pmSd3,pmMean5,pmSd5,pmSderr5,pmDL3,Sdrel5)
  pmStat5$time        <- NA

  ## Make up for output
  ## pmStat6          <- pmStat5[,colnames(pmStat5)!="time"] # drop column of `time'
  pmlame6             <- rbind(pmlame5,pmStat5) # Merge ion/ref and stat

  return(pmlame6)
}
