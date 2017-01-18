#' @title Estimate element abundances from ion-intensity obtained by
#'   mass spectrometry.
#'
#' @description Estimate element abundances from ion-intensity
#'   obtained by mass spectrometry.  Main inputs are element abundance
#'   of internal-reference element, ionic_ratio, and ionic_yield.
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
#'   \item{[Li]        <- ionic_ratio/ionic_yield * [Si]/m!(Si29) * m!(Li7)}
#' }
#'
#' @param pmlame A pmlame with internal-reference element such for Si
#'   and Ca.  Reduce SiO2 to Si in advance using
#'   \link{cbk.lame.reduce}.
#' @param ionic_ratio A pmlame-like ion signal intensities relative to
#'   internal-reference isotope.  For example, I(Li7)/I(Si29).
#' @param ionic_yield Relative sensitivities of element that were
#'   determined by analyses of several reference materials.
#' @param isoref Name of internal-reference isotope such as 'Si29'.
#' @return A pmlame of element abundances.
#' @export
#' @examples
#' ionic_yield <- data.frame(row.names=c("ionic_yield"), Li7=c(1.027), B11=c(1.76), Si29=c(1), La139=c(77.6))
#' ionic_ratio <- data.frame(row.names=c('ref_cpx_klb1@1','ref_cpx_klb1@2','trc_meso_allende@10'),Li7=c(5.56389e-04,4.90334e-04,7.02148e-05),B11=c(5.88269e-05,8.64064e-05,1.44872e-04),Si29=c(1,1,1),La139=c(0.000340813,0.000281243,0.000708434))
#' pmlame0     <- data.frame(row.names=c('ref_cpx_klb1@1','ref_cpx_klb1@2','trc_meso_allende@10'),SiO2=c(520000,520000,600000))
#' pmlame1     <- cbk.lame.reduce(pmlame0)
#' cbk.lame.atomify(pmlame1,ionic_ratio,ionic_yield,isoref='Si29')
cbk.lame.atomify <- function(pmlame,ionic_ratio,ionic_yield,isoref='Si29') {
  ## Reduce SiO2 to Si
  ## pmlame        <- cbk.lame.reduce(pmlame)

  isomeas           <- intersect(colnames(ionic_ratio),colnames(ionic_yield))
  ## isomeas        <- c('Li7','B11','Si29','La139')
  pseudowt          <- cbk.iso(isomeas,'pseudo.atomic.weight') # m(Li)/R(Li7) or m(Si)/R(Si29)
  stonelist         <- intersect(rownames(ionic_ratio),rownames(pmlame))
  ## stonelist      <- c('ref_gl_tahiti@2','ref_gl_tahiti@3','ref_gl_tahiti@4','trc_meso_allende@10')

  ## Setup lames and estimate atomic ratio
  ionic_yield1      <- ionic_yield[,isomeas]
  ionic_ratio1      <- ionic_ratio[stonelist,isomeas]
  atomic_ratio1     <- cbk.lame.normalize(ionic_ratio1,ionic_yield1)

  ## Format chem-data of reference element
  concref           <- pmlame[stonelist,cbk.iso(isoref,'symbol'),drop=FALSE]
  concref1          <- cbk.lame.rep(concref,length(isomeas))

  ## Estimate element-abundance from atomic-ratio
  pmlame1           <- cbk.lame.normalize(atomic_ratio1, 1/pseudowt) * concref1 / pseudowt[isoref]
  colnames(pmlame1) <- cbk.iso(colnames(pmlame1),'symbol') # element-name instead of iso-name

  return(pmlame1)
}
