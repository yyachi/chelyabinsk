#' @title Return properties of isotope
#'
#' @description Return properties of isotope.  It is in a form of
#'   pmlame.
#'
#' @param isomeas A name of isotope such as `Li7' and `Rb87'.
#' @param property A name of PROPERTY that is one of `proton',
#'   `nucleon', `weight', `ratio', `symbol', or `pseudo.atomic.weight'.
#' @return A scalar of property or pmlame of isotope table
#' @seealso \code{\link{cbk.periodic}}, isotope.m, isotope_constant.m
#' @export
#' @examples
#' cbk.iso()
#' cbk.iso('Li7')
#' cbk.iso('Li7','ratio')
cbk.iso <- function(isomeas=NULL,property='weight'){
  isolame0 <- cbk.read.tblame(cbk.path("isotope-tblame0.csv"),verbose=FALSE)

  if (property %in% c('proton','atomicnumber','atomic.number',
                      'atomic number','Z')) {
    property <- 'proton'
  }

  if (property %in% c('nucleon','iso','isonum')) {
    property <- 'nucleon'
  }

  if (property %in% c('weight','m','atomicweight','atomic.weight',
                      'atomic weight','mass','atomicmass',
                      'atomic.mass','atomic mass')) {
    property <- 'weight'
  }

  if (property %in% c('ratio','ab','abundance','r')) {
    property <- 'ratio'
  }

  if (property %in% c('symbol','element','sym')) {
    property <- 'symbol'
  }

  if (property %in% c('pseudo.atomic.weight','pseudo atomic weight',
                      'pseudo atomicweight','pseudo.atomicweight')) {
    property <- 'pseudo.atomic.weight'
  }

  if (is.null(isomeas)) {
    return(isolame0)
  } else if (property == 'pseudo.atomic.weight') {
    myvect            <- isolame0[isolame0[isomeas,'symbol'],'weight']/isolame0[isomeas,'ratio']
    names(myvect)     <- isomeas
    return(myvect)
  } else {
    myvect            <- isolame0[isomeas,property]
    names(myvect)     <- isomeas
    return(myvect)
  }
}
