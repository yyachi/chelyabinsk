#' @title Estimate element abundances from ion-intensity obtained by
#'   mass spectrometry.
#'
#' @description Estimate element abundances from ion-intensity
#'   obtained by mass spectrometry.  Main inputs are element abundance
#'   of internal-reference element, ionic_ratio, and ionic_yield.
#'   This function accepts a pair of mean and error of ionic ratio
#'   such in columns `Li7' and 'Li7_error' and yield a pair of mean
#'   and error of element abundance such in columns `Li' and
#'   `Li_error'.
#'
#' @details This function estimate trace-element abundances using
#'   following equations.  Note that `m!' denotes `pseudo
#'   atomic-weight' that corresponds to atomic-weight of an isotope at
#'   virtual world where rest of isotopes do not exist (number of
#'   other isotopes are zero) but total element abundance and number
#'   of the isotope remain the same.
#'
#' \itemize{
#'   \item{ionic_yield  = ionic_ratio / atomic_ratio}
#'   \item{ionic_ratio  = I(Li7+)     / I(Si29+)}
#'   \item{atomic_ratio = [Li7]       / [Si29]}
#'   \item{[Li7]        = [Li]/m!(Li7)}
#'   \item{[Si29]       = [Si]/m!(Si29)}
#'   \item{m!(Li7)      = m(Li)/R(Li7)}
#'   \item{m!(Si9)      = m(Si)/R(Si29)}
#'   \item{[Li]         <- ionic_ratio/ionic_yield * [Si]/m!(Si29) * m!(Li7)}
#' }
#'
#' @param pmlame A pmlame that includes internal-reference element
#'   such for Si with row of acq and column of chem [g/g].  Do not
#'   forget to reduce in advance using \link{cbk.lame.reduce}.
#' @param ionic_ratio A pseudo-pmlame of ion intensity relative to
#'   internal-reference isotope with row of acq and column of isomeas
#'   [cps/cps].  Rownames should be identical to that of `pmlame'.
#'   This is like array of I(Li7)/I(Si29), ..., I(Sr88)/I(Si29).
#' @param ionic_yield Relative sensitivities of element that were
#'   determined by analyses of several reference materials.
#' @param isoref Name of internal-reference isotope such as 'Si29'.
#' @param verbose Output debug info (default: FALSE).
#' @return A pmlame of element abundances.
#' @export
#' @examples
#' ionic_yield <- data.frame(row.names=c("ionic_yield"), Li7=c(1.027), B11=c(1.76), Si29=c(1), La139=c(77.6))
#' ionic_ratio <- data.frame(row.names=c('ref_cpx_klb1@1','ref_cpx_klb1@2','trc_meso_allende@10'),Li7=c(5.56389e-04,4.90334e-04,7.02148e-05),B11=c(5.88269e-05,8.64064e-05,1.44872e-04),Si29=c(1,1,1),La139=c(0.000340813,0.000281243,0.000708434))
#' pmlame0     <- data.frame(row.names=c('ref_cpx_klb1@1','ref_cpx_klb1@2','trc_meso_allende@10'),SiO2=c(520000,520000,600000))
#' pmlame      <- cbk.lame.reduce(pmlame0)
#' cbk.lame.atomify(pmlame,ionic_ratio,ionic_yield,isoref='Si29',verbose=TRUE)
cbk.lame.atomify <- function(pmlame,ionic_ratio,ionic_yield,isoref='Si29',verbose=FALSE) {
  ##* Reduce SiO2 to Si
  ## pmlame         <- cbk.lame.reduce(pmlame)

  if (verbose) {
    cat(file=stderr(),"cbk.lame.atomify:54: pmlame <-",cbk.lame.dump(pmlame,show=F),"\n")
    cat(file=stderr(),"cbk.lame.atomify:55: ionic_ratio <-",cbk.lame.dump(ionic_ratio,show=F),"\n")
    cat(file=stderr(),"cbk.lame.atomify:56: ionic_yield <-",cbk.lame.dump(ionic_yield,show=F),"\n")
  }

  ##* Obtain list of items
  ## isomeas        <- c('Li7','B11','Si29','La139')
  ## isomeas        <- grep("_error",colnames(ionic_ratio),value=T,invert=T) # exclude _error
  isomeas           <- colnames(cbk.lame.regulate(ionic_ratio,mean=T,error=F,extra=F))
  isomeas           <- intersect(isomeas,colnames(ionic_yield))
  pseudowt          <- as.data.frame(t(cbk.iso(isomeas,'pseudo.atomic.weight'))) # m(Li)/R(Li7) or m(Si)/R(Si29)
  ## acqlist        <- c('ref_gl_tahiti@2','ref_gl_tahiti@3','ref_gl_tahiti@4','trc_meso_allende@10')
  acqlist           <- intersect(rownames(ionic_ratio),rownames(pmlame))

  ##* Format chem-data of reference element
  reflame           <- pmlame[acqlist,cbk.iso(isoref,'symbol'),drop=FALSE]
  reflame1          <- cbk.lame.rep(reflame,length(isomeas))

  ##* Setup lames and estimate atomic ratio
  ionic_yield1      <- ionic_yield[,isomeas]
  ionic_ratio1      <- cbk.lame.regulate(ionic_ratio,mean=T,error=F,extra=F,chem=isomeas)[acqlist,] # ionic_ratio[acqlist,isomeas]
  ionic_error1      <- cbk.lame.fetch.error(ionic_ratio,chem=isomeas)[acqlist,] # errorlame
  atomic_ratio1     <- cbk.lame.normalize(ionic_ratio1,ionic_yield1)

  ##* Estimate element-abundance from atomic-ratio
  if (verbose) {
    cat(file=stderr(),"cbk.lame.atomify:81: ionic_yield1 <-",cbk.lame.dump(ionic_yield1,show=F),"\n")
    cat(file=stderr(),"cbk.lame.atomify:81: ionic_ratio1 <-",cbk.lame.dump(ionic_ratio1,show=F),"\n")
    cat(file=stderr(),"cbk.lame.atomify:82: atomic_ratio1 <-",cbk.lame.dump(atomic_ratio1,show=F),"\n")
    cat(file=stderr(),"cbk.lame.atomify:83: pseudowt <-",cbk.lame.dump(pseudowt,show=F),"\n")
    cat(file=stderr(),"cbk.lame.atomify:84: reflame1 <-",cbk.lame.dump(reflame1,show=F),"\n")
    cat(file=stderr(),"cbk.lame.atomify:85: isoref <-",cbk.lame.dump(isoref,show=F),"\n")
  }
  pmlame1           <- cbk.lame.normalize(atomic_ratio1, 1/pseudowt, verbose=verbose) * reflame1 / pseudowt[,isoref]

  ##* Rename label from Li7 (incorrect) to Li (correct)
  colnames(pmlame1) <- cbk.iso(colnames(pmlame1),'symbol') # from 'Li7' to 'Li'

  ##* Do similar things for error if any
  if (ncol(ionic_error1) > 0) {
    if (verbose) {
      cat(file=stderr(),"cbk.lame.atomify:82: colnames(ionic_error1) # =>",colnames(ionic_error1),"\n")
      cat(file=stderr(),"cbk.lame.atomify:83: rownames(ionic_error1) # =>",rownames(ionic_error1),"\n")
    }
    atomic_error1        <- cbk.lame.normalize(ionic_error1,ionic_yield1) # errorlame
    errorlame1           <- cbk.lame.normalize(atomic_error1, 1/pseudowt) * reflame1 / pseudowt[,isoref] # errorlame
    colnames(errorlame1) <- cbk.iso(colnames(errorlame1),'symbol') # from 'Li7' to 'Li'

    pmlame1              <- cbk.lame.merge.error(pmlame1,errorlame1)
  }

  return(pmlame1)
}
