#' @title Estimate element abundances from ion-intensity obtained by
#'   mass spectrometry.
#'
#' @description Estimate element abundances from ion-intensity
#'   obtained by mass spectrometry.  Inputs are element abundance of
#'   internal-reference element, ionic_ratio, and ion_yield.
#' 
#' @details
#'   ion_yield     ≡ ionic_ratio / atomic_ratio
#'   ionic_ratio   ≡ I(Li7+)      / I(Si29+)
#'   atomic_ratio  ≡ [Li7]        / [Si29]
#'   [Li7]         ≡ [Li]/m*(Li7)
#'   [Si29]        ≡ [Si]/m*(Si29)
#'   m*(Li7)       ≡ m(Li)/R(Li7)
#'   m*(Si9)       ≡ m(Si)/R(Si29)
#'   [Li]          <- ionic_ratio/ion_yield * [Si]/m*(Si29) * m*(Li7)
#' 
#' @param pmlame0 A pmlame with internal-referene element such for Si
#'   and Ca.  Note that you have to reduce SiO2 in advance.
#' @param ionic_ratio0 A pmlame-like ion signal intensities relative
#'   to internal-reference isotope.  For example, I(Li7)/I(Si29).
#' @param ion_yield0 Sensitivities of element that were determined by
#'   analyses of several reference materials.
#' @return A pmlame of element abundances.
#' @export
cbk.lame.atomify <- function(pmlame0,ionic_ratio0,ion_yield0) {
  ## Reduce SiO2 to Si
  ## pmlame1           <- cbk.lame.reduce(pmlame0)

  isomeas           <- intersect(colnames(ionic_ratio0),colnames(ion_yield0))
  ## isomeas        <- c('Li7','B11','Si29','La139','Ce140')
  pseudowt          <- cbk.iso(isomeas,'pseudo.atomic.weight') # m(Li)/R(Li7) or m(Si)/R(Si29)
  stonelist         <- intersect(rownames(ionic_ratio0),rownames(pmlame1))
  ## stonelist      <- c('ref_gl_tahiti@2','ref_gl_tahiti@3','ref_gl_tahiti@4','trc_meso_allende@10')

  ## Setup lames and estimate atomic ratio
  ion_yield         <- ion_yield0[,isomeas]
  ionic_ratio       <- ionic_ratio0[stonelist,isomeas]
  atomic_ratio      <- cbk.lame.normalize(ionic_ratio, ion_yield)

  ## Format chem-data of reference element
  concref0          <- pmlame1[stonelist,cbk.iso(isoref,'symbol'),drop=FALSE]
  concref           <- cbk.lame.rep(concref0,length(isomeas))

  ## Estiamte element-abundance from atomic-ratio
  pmlame8           <- cbk.lame.normalize(atomic_ratio, 1/pseudowt) * concref / pseudowt[isoref]
  colnames(pmlame8) <- cbk.iso(colnames(pmlame8),'symbol') # element-name instead of iso-name
  
  return(pmlame8)
}
