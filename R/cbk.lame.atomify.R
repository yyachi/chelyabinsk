#' @title Estimate element abundances from ion-intensity obtained by
#'   mass spectrometry.
#'
#' @description Estimate element abundances from ion-intensity
#'   obtained by mass spectrometry.  Inputs are element abundance of
#'   internal-reference element, ionic_ratio, and ion_yield.
#'
#' @details
#' \itemize{
#'   \item{ion_yield    = ionic_ratio / atomic_ratio}
#'   \item{ionic_ratio  = I(Li7+)     / I(Si29+)}
#'   \item{atomic_ratio = [Li7]       / [Si29]}
#'   \item{[Li7]        = [Li]/m!(Li7)}
#'   \item{[Si29]       = [Si]/m!(Si29)}
#'   \item{m!(Li7)      = m(Li)/R(Li7)}
#'   \item{m!(Si9)      = m(Si)/R(Si29)}
#'   \item{[Li]        <- ionic_ratio/ion_yield * [Si]/m!(Si29) * m!(Li7)}
#' }
#'
#' @param pmlame A pmlame with internal-referene element such for Si
#'   and Ca.  Note that you have to reduce SiO2 in advance.
#' @param ionic_ratio A pmlame-like ion signal intensities relative
#'   to internal-reference isotope.  For example, I(Li7)/I(Si29).
#' @param ion_yield Sensitivities of element that were determined by
#'   analyses of several reference materials.
#' @return A pmlame of element abundances.
#' @export
cbk.lame.atomify <- function(pmlame,ionic_ratio,ion_yield) {
  ## Reduce SiO2 to Si
  ## pmlame        <- cbk.lame.reduce(pmlame)

  isomeas           <- intersect(colnames(ionic_ratio),colnames(ion_yield))
  ## isomeas        <- c('Li7','B11','Si29','La139','Ce140')
  pseudowt          <- cbk.iso(isomeas,'pseudo.atomic.weight') # m(Li)/R(Li7) or m(Si)/R(Si29)
  stonelist         <- intersect(rownames(ionic_ratio),rownames(pmlame))
  ## stonelist      <- c('ref_gl_tahiti@2','ref_gl_tahiti@3','ref_gl_tahiti@4','trc_meso_allende@10')

  ## Setup lames and estimate atomic ratio
  ion_yield1        <- ion_yield[,isomeas]
  ionic_ratio1      <- ionic_ratio[stonelist,isomeas]
  atomic_ratio1     <- cbk.lame.normalize(ionic_ratio1, ion_yield1)

  ## Format chem-data of reference element
  concref           <- pmlame[stonelist,cbk.iso(isoref,'symbol'),drop=FALSE]
  concref1          <- cbk.lame.rep(concref,length(isomeas))

  ## Estiamte element-abundance from atomic-ratio
  pmlame1           <- cbk.lame.normalize(atomic_ratio1, 1/pseudowt) * concref1 / pseudowt[isoref]
  colnames(pmlame1) <- cbk.iso(colnames(pmlame1),'symbol') # element-name instead of iso-name

  return(pmlame1)
}
