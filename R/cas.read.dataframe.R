#' Convert dataframe with label to vector with label
#' @param dataframefile A flipped csvfile
#' @param tableunit Preferred unit, that is one of c("none","g/g","wt%","cg/g","%","permil","mg/g","ppm","ug/g","ppb","ng/g","pg/g")
#' @return A dataframe
#' @export
#' @examples
#' $ casteml download -R 20130528105235-594267 > 20130528105235-594267.pml
#' $ casteml convert -f dataframe -c trace 20130528105235-594267.pml > 20130528105235-594267.dataframe
#' R> tbl0 <- cas.read.dataframe("20130528105235-594267.dataframe","ppm")
cas.read.dataframe <- function(dataframefile,tableunit="none"){
  #' ----------------
  #' unit definition
  #' ----------------
  convector        <- c( 1,     1,    100, 100,    100,1000,    1000,  1000000,1000000,1000000000,1000000000,1000000000000)
  names(convector) <- c("none","g/g","wt%","cg/g","%","permil","mg/g","ppm",  "ug/g", "ppb",     "ng/g",    "pg/g")
  
  #' ----------------
  #' load dataset from csvfile with unit
  #' ----------------
  tblin <- read.csv(dataframefile,row.names=1,header=T,stringsAsFactors=F)
  if ('unit' %in% colnames(tblin)) {
    factor <- convector[tblin[,'unit']]
    names(factor) <- rownames(tblin)
    factor[is.na(factor)] <- 1
    tbl0 <- t(tblin[colnames(tblin) != 'unit'] / factor) * convector[tableunit]
  } else {
    tbl0 <- t(tblin)
  }
  return(tbl0)
}
