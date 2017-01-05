#' @title Convert laicpqms-type QTEGRA.CSV by Analyte G2 with iCAP-Q
#'   to ion-type TBLAME.CSV
#'
#' @description Convert laicpqms-type QTEGRA.CSV by Analyte G2 with
#'   iCAP-Q to ion-type TBLAME.CSV.  Filter metadata out and append
#'   column of cycle number.
#' 
#'   This does NOT produce IONML but does imcomplete IONML that is
#'   ion-type TBLAME.csv.
#'
#' @param file Name of laicpqms-type QTEGRA.CSV exported form Qtegra
#' @param outfile Name of ion-type TBLAME.csv file that will be
#'   created
#' @param force Flag to force convert again
#' @return Name of ion-type TBLAME.csv file that was saved
#' @seealso ionml-laicpqms.plx
#' @export
#' @examples
#' file <- cbk.path("ref_cpx_klb1@1.qtg")
#' ionml.convert.laicpqms(file,outfile=tempfile(fileext=".ion"))
ionml.convert.laicpqms <- function(file,outfile=NULL,force=FALSE) {
  ## Guess extension for input on omit
  if (grepl("",tools::file_ext(file))) {
    file <- paste0(tools::file_path_sans_ext(file),".qtg")
  }

  ## Set appropriate extension for output
  if(is.null(outfile)){
    outfile <- paste0(tools::file_path_sans_ext(file),".ion")
  }

  ## Convert file only when destination does not exist
  if (force || !file.exists(outfile)) {
    ## INPUT
    con <- file(file,open="r")
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
        writeLines(sprintf("rownames,%s",line),con)
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
