#' @title Return properties of isotope
#'
#' @description Return properties of isotope.  It is in a form of
#'   pmlame.
#'
#' @param isomeas A name of isotope such as `Li7' and `Rb87'.
#' @param property A name of PROPERTY that is one of `proton',
#'   `nucleon', `mass', or `ratio'
#' @return A scalar of property or pmlame of isotope table
#' @seealso \code{\link{cbk.periodic}}, isotope.m, isotope_constant.m
#' @export
#' @examples
#' cbk.iso()
#' cbk.iso('Li7')
#' cbk.iso('Li7','ratio')
cbk.iso <- function(isomeas=NULL,property='mass'){
  isolame0 <- cbk.read.tblame(cbk.path("isotope-tblame0.csv"),verbose=FALSE)

  if (property %in% c('atomicnumber','Z','proton')) {
    property <- 'proton'
  }

  if (property %in% c('iso','isonum','nucleon')) {
    property <- 'nucleon'
  }

  if (property %in% c('m','mass','weight')) {
    property <- 'mass'
  }

  if (property %in% c('ab','abundance','r','ratio')) {
    property <- 'ratio'
  }

  if (is.null(isomeas)) {
    return(isolame0)
  } else {
    return(isolame0[isomeas,property])
  }
}
