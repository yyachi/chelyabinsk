#' @title Read csvfile created by casteml convert with column `unit'
#'
#' @description Read csvfile with column `unit' created by
#'   \code{casteml convert --format dflame}.  This is low-level
#'   function and users are not encourage to call this but
#'   cbk.read.casteml().
#'
#' @details This internally calls
#'   \code{read.csv(dflame.csv,row.names=1,header=T,stringsAsFactors=F)},
#'   take out column of `unit' and normalized by the `unit' column.
#' @param dflame.csv A csvfile with columns of stone and rows of
#'   element.  abundances, with 2nd column `unit'.
#' @param tableunit Output unit that will be resolved by
#'   \link{cbk.convector} (default: "none").
#' @param verbose Output debug info (default: TRUE).
#' @param force Force read csvfile with duplicated acquisitions
#'   (default: TRUE).
#' @return A pmlame with unit organized.
#' @seealso \code{\link{cbk.download.casteml}}, \code{casteml
#'   convert}, \url{https://github.com/misasa/casteml}, and
#'   \code{\link{cbk.convector}}.
#' @export
#' @examples
#' pmlfile    <- cbk.download.casteml("20081202172326.hkitagawa")
#' dflame.csv <- cbk.convert.casteml(pmlfile,category="trace")
#' pmlame     <- cbk.read.dflame(dflame.csv,"ppm",force=TRUE)
#'
#' dflame.csv <- cbk.path("20081202172326.hkitagawa_trace.dflame")
#' message(sprintf("The dflame.csv is located at |%s|.",dflame.csv))
#' pmlame     <- cbk.read.dflame(dflame.csv,"ppm")
#'
#' dflame.csv <- cbk.path("ref1-dflame0.csv")
#' message(sprintf("The dflame.csv is located at |%s|.",dflame.csv))
#' pmlame     <- cbk.read.dflame(dflame.csv,"ppm")
#'
#' dflame.csv <- cbk.path("periodic-dflame0.csv")
#' message(sprintf("The dflame.csv is located at |%s|.",dflame.csv))
#' pmlame     <- cbk.read.dflame(dflame.csv)
cbk.read.dflame <- function(dflame.csv,tableunit="none",verbose=TRUE,force=TRUE){

  if (verbose) {
    cat(file=stderr(),"cbk.read.dflame:32: dflame.csv # =>",dflame.csv,"\n")
  }

  ## EXAMPLES
  ## $ casteml download -R 20130528105235-594267 > 20130528105235-594267.pml
  ## $ casteml convert -f dflame -c trace 20130528105235-594267.pml > 20081202172326.hkitagawa_trace.dflame
  ## R> pmlame <- cbk.read.dflame("20130528105235-594267.dflame","ppm")

  ## qmlame <- read.csv(dflame.csv,row.names=1,header=T,stringsAsFactors=F)
  qmlame <- read.csv(dflame.csv,row.names=1,header=T,stringsAsFactors=F,check.names=F)
  ## qmlame <- qmlame[unique(colnames(qmlame))]

  colnames(qmlame)  <- gsub("-","_",colnames(qmlame))
  if (any(duplicated(colnames(qmlame)))){
    dupstone <- colnames(qmlame)[duplicated(colnames(qmlame))]
    if (force) {
      idx <- which(duplicated(colnames(qmlame)))
      for(ii in 1:length(dupstone)) {  ## Rename duplicated stones
        if ("sample_id" %in% rownames(qmlame)) {
          new_name <- paste0(dupstone[ii]," <stone ",qmlame["sample_id",idx[ii]],">",idx[ii])
        } else {
          new_name <- paste0(dupstone[ii]," <",Sys.Date(),">")
        }
        colnames(qmlame)[idx[ii]] <- new_name
        cat(file=stderr(),"Warning: There were multiply-defined analyses. `",dupstone[ii],"' was renamed to `",new_name,"'\n")
      }
    } else {
      stop(cat(file=stderr(),"Error: Duplicated stone is found: stone # =>",dupstone,"
       If desired, force option can rename it automatically.\n"))
    }
  }

  if ('unit' %in% colnames(qmlame)) {
    isomeas_in               <- rownames(qmlame)
    unit_in                  <- qmlame[,"unit"]

    ## Create lookup-table
    datap                    <- !grepl("_error$",isomeas_in)
    tblunit                  <- unit_in[datap]
    names(tblunit)           <- isomeas_in[datap]

    ## Have unit for isomeas
    chemeas                  <- gsub("_error","",isomeas_in)
    names(chemeas)           <-isomeas_in
    unit                     <- tblunit[chemeas]
    names(unit)              <-isomeas_in

    rownames(qmlame)[datap] <- chemeas[datap]
    qmlame[,"unit"]         <- unit
    ## qmlame                   <- cbind(qmlame, unit=unit)

    rowSTR <- intersect(rownames(qmlame),c("image_path","sample_id","image_id","remark"))
    if (length(rowSTR)==0) {
      factor                <- cbk.convector(as.character(qmlame[,'unit']))
      names(factor)         <- rownames(qmlame)
      factor[is.na(factor)] <- 1
      pmlame                <- as.data.frame(t(qmlame[colnames(qmlame) != 'unit'] / factor)) * cbk.convector(tableunit)  
    } else {
      qmlameSTR             <- qmlame[rowSTR,]
      ## qmlameNUM             <- qmlame[rownames(qmlame) != rowSTR,]
      qmlameNUM             <- qmlame[setdiff(rownames(qmlame),rowSTR),]
      factor                <- cbk.convector(as.character(qmlameNUM[,'unit']))
      names(factor)         <- rownames(qmlameNUM)
      factor[is.na(factor)] <- 1
      qmlameNUM[,'unit']    <- NA
      pmlameNUM             <- as.data.frame(t(apply(qmlameNUM,2,function(x) as.numeric(x) / factor * cbk.convector(tableunit))))
      pmlame0               <- cbind(pmlameNUM,t(qmlameSTR))
      pmlame                <- pmlame0[rownames(pmlame0) != 'unit',]
    }
  } else { # without unit column
    pmlame <- as.data.frame(t(qmlame))
  }
  return(pmlame)
}
