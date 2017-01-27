#' @title Read csvfile created by casteml convert with column `unit'
#'
#' @description Read csvfile with row `unit', that is also referred as
#'   `tblame'.
#'
#' @details This internally calls \code{\link{read.csv}}, take out row
#'   of `unit' and normalized by the `unit' row.
#' @param tblame A csvfile with columns of chem and rows of stone,
#'   with 2nd row `unit'
#' @param tableunit Output unit that will be resolved by
#'   \link{cbk.convector} (default: "none").
#' @param verbose Output debug info (default: TRUE).
#' @return A pmlame with unit organized.
#' @seealso \code{\link{cbk.read.dflame}},
#'   \url{https://github.com/misasa/casteml}, and
#'   \code{\link{cbk.convector}}.
#' @export
cbk.read.tblame <- function(tblame,tableunit="none",verbose=TRUE){

  ## cat(file=stderr(),"cbk.read.tblame:20: tblame is |",tblame,"|\n")
  if (verbose) {
    cat(file=stderr(),"cbk.read.tblame:23: tblame # =>",tblame,"\n")
  }

  ## pmlame <- read.csv(tblame,row.names=1,header=T,stringsAsFactors=F)
  pmlame0 <- read.csv(tblame,row.names=1,header=T,stringsAsFactors=F,check.names=F)

  ## pattern_colname <- "^([A-Za-z].*) ?(_error|\\(.*\\))"
  ## chemlist        <- colnames(pmlame0)
  ## isomeas         <- gsub(pattern_colname,"\\1",chemlist) # Li (ppm) -> Li
  ## unit            <- gsub(pattern_colname,"\\2",chemlist) # Li (ppm) -> (ppm)
  ## isomeas         <- gsub(" ","",isomeas) # remove empty space
  ## if (any(grepl("_error",unit))) {
  ##   for(ii in 1:length(chemlist)) {
  ##     if (unit[ii] == "_error") {
  ##       unit[ii]     <- unit[which(grepl(isomeas[ii], chemlist)[-ii])]
  ##     } else {
  ##       chemlist[ii] <- isomeas[ii]
  ##     }
  ##   }
  ##   colnames(pmlame0) <- chemlist
  ##   pmlame            <- rbind(pmlame0, unit=gsub("[()]","",unit))
  ## } else {
  ##   pmlame <- pmlame0
  ## }
  isomeas_in      <- colnames(pmlame0)
  pattern_colname <- "^([A-Za-z].*) ?(_error|\\(.*\\))"
  isomeas         <- gsub(pattern_colname,"\\1",isomeas_in) # Li (ppm) -> Li
  isomeas         <- gsub("[ ]","",isomeas)
  unit_in         <- gsub(pattern_colname,"\\2",isomeas_in) # Li (ppm) -> (ppm)
  unit_in         <- gsub("[()]","",unit_in)

  if (any(grepl("_error",unit_in))) {
    ## Create lookup-table
    datap           <- !grepl("_error$",isomeas_in)
    tblunit         <- unit_in[datap]
    names(tblunit)  <- isomeas[datap]

    ## Have unit for isomeas
    unit            <- tblunit[isomeas]
    names(unit)     <- isomeas_in

    colnames(pmlame0)[datap] <- isomeas[datap]
    pmlame                   <- rbind(pmlame0, unit=unit)
  } else {
    pmlame <- pmlame0
  }
  
  if ('unit' %in% rownames(pmlame)) {
    colSTR <- intersect(colnames(pmlame),c("image_path","sample_id","image_id","remark"))
    if (length(colSTR)==0) {
      factor                <- cbk.convector(as.matrix(pmlame['unit',]))
      names(factor)         <- colnames(pmlame)
      factor[is.na(factor)] <- 1
      pmlame                <- as.data.frame(t(apply(pmlame[rownames(pmlame) != 'unit',],1,function(x) as.numeric(x) / factor ))* cbk.convector(tableunit))
    } else {
      pmlameSTR             <- pmlame[,colSTR,drop=FALSE]
      pmlameNUM             <- pmlame[,colnames(pmlame) != colSTR,drop=FALSE]
      factor                <- cbk.convector(as.matrix(pmlameNUM['unit',]))
      names(factor)         <- colnames(pmlameNUM)
      factor[is.na(factor)] <- 1
      pmlameNUM             <- as.data.frame(t(apply(pmlameNUM,1,function(x) as.numeric(x) / factor )* cbk.convector(tableunit)))
      pmlame0               <- cbind(pmlameNUM,pmlameSTR)
      pmlame                <- pmlame0[rownames(pmlame0) != 'unit',]
    }
  }
  return(pmlame)
}
