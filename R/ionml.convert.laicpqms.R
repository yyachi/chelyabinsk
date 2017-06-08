#' @title Convert LAICPQMS-type QTEGRACSV by Analyte G2 with iCAP-Q
#'   to IONCSV
#'
#' @description Convert LAICPQMS-type QTEGRACSV by Analyte G2 with
#'   iCAP-Q to IONCSV.  Filter metadata out and append column of cycle
#'   number.
#'
#'   This program assumes extensions of LAICPQMS-type QTEGRACSV and
#'   IONCSV to be `.csv' and `.ion', respectively.
#'
#'   The IONCSV consists of columns of time and ion intensities.  The
#'   first column of each line should be number of `cycle'.  Colname
#'   of the IONCSV should be `time' and name of element followed by
#'   atomic weight (`Si29' instead of `29Si').
#'
#' @param acqfile Name of LAICPQMS-type QTEGRACSV exported form Qtegra
#' @param outfile Name of IONCSV that will be created
#' @param force Flag to force convert again
#' @return Name of IONCSV that was created
#' @seealso ionml-laicpqms.plx
#' @export
#' @examples
#' acqfile <- cbk.path("ref_cpx_klb1@1.csv")
#' ionml.convert.laicpqms(acqfile,outfile=tempfile(fileext=".ion"))
ionml.convert.laicpqms <- function(acqfile,outfile=NULL,force=FALSE) {

  ## Guess extension of acqfile on omit
  if (grepl("",tools::file_ext(acqfile))) {
    acqfile <- paste0(tools::file_path_sans_ext(acqfile),".csv")
  }

  cat(file=stderr(),"ionml.convert.laicpqms:28: acqfile # =>",acqfile,"\n")

  ## Set name of outfile by default
  if(is.null(outfile)){
    outfile <- paste0(tools::file_path_sans_ext(acqfile),".ion")
  }

  ## Convert file only when destination does not exist
  if (force || !file.exists(outfile)) {
    ## INPUT
    con <- file(acqfile,open="r")
    line_stack <-readLines(con)
    close(con)

    ## OUTPUT
    con <- file(outfile,open="w")
    bodyp <- 0
    dwellp <- FALSE
    for (ii in 1:length(line_stack)){
      line <- line_stack[ii]

      line <- sub(",$","",line)
      if (grepl("^\\s*$",line)) next

      if (grepl("^Time",line)) {
        bodyp <- 1
        line <- sub("Time","time",line)
        line <- gsub("([0-9]+)([A-Z][a-z]?)","\\2\\1",line) # 151Eu -> Eu151
        writeLines(sprintf("cycle,%s",line),con)
      } else {
        dwellp <- grepl("dwell",line)
        if (bodyp && !dwellp) {
          writeLines(sprintf("%d,%s",bodyp,line),con)
          bodyp <- bodyp + 1
        }
      }
    }
    close(con)
  }
  return(outfile)
}
