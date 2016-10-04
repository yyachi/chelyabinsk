#' @title Convert dataframe with label to vector with label
#'
#' @description Convert dataframe with label to vector with label.  It
#'   seems that this conversion is necessary for calcuration.
#' 
#' @details This is written at initial stage of development.  May be
#'   not that useful.
#' @param casdfm A dataframe with label
#' @return A vector with label
#' @export
#' @examples
#' cbk.vector(data.frame(row.names=c("density"), ol=c(3.300), pig=c(3.400), WR=c(3.3)))
cbk.vector <- function(casdfm){
  ### EXAMPLES
  ### DD <- data.frame(row.names=c("mode","density"), ol=c(0.525,3.300), pig=c(0.353,3.400), WR=c(1.0,3.3))
  ### > DD
  ###            ol   pig  WR
  ### mode    0.525 0.353 1.0
  ### density 3.300 3.400 3.3
  ### > str(DD)
  ### 'data.frame':	2 obs. of  3 variables:
  ###  $ ol : num  0.525 3.3
  ###  $ pig: num  0.353 3.4
  ###  $ WR : num  1 3.3
  ### > DD["density",]
  ###          ol pig  WR
  ### density 3.3 3.4 3.3
  ### > cbk.vector(DD["density",])
  ###  ol pig  WR
  ### 3.3 3.4 3.3
  ### > VV <- cbk.vector(DD["density",])
  ### > VV
  ###  ol pig  WR
  ### 3.3 3.4 3.3
  ### > str(VV)
  ###  Named num [1:3] 3.3 3.4 3.3
  ###  - attr(*, "names")= chr [1:3] "ol" "pig" "WR"

  ## Primary target is a dataframe with single row
  casvector        <- as.numeric(casdfm)
  names(casvector) <- names(casdfm)
  return(casvector)
}
