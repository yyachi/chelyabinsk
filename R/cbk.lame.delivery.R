#' @title Estimate delivery rate of ion from a pit by laser abbration
#' @description Estimate delivery rate of ion from a pit by laser
#'   abbration.  Although dimension of a pit is on meter [m],
#'   internaly, they are are converted into centi-meter [cm].
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param intlame A pseudo-pmlame of ion intensity with row of stone
#'   and column of isomeas [cps].
#' @param radius Radius of a pit [m] (default: 12.5e-6).
#' @param density Density of solid target [g/cc] (default: 3).
#' @param depth Depth of a pit [m] (default: 30e-6).
#' @param t Period of irradiation [s] (default: 40).
#' @return Delivery rate from solid target to detector.
#' @export
#' @examples
#' intlame0 <- data.frame(row.names=c("ref_gl_tahiti@1","ref_cpx_klb1@1"),Si29=c(3.85e+05,3.95e+05),Li7=c(1.87e+03,1.73e+02),Sr88=c(3.24e+04,1.50e+04))
#' pmlame0 <- data.frame(row.names=c("ref-gl-tahiti","ref-cpx-klb1"),SiO2=c(0.590,0.520),Li=c(20.8e-06,1.37e-06),Sr=c(107e-06,36.1e-06))
#' pmlame1 <- cbk.lame.reduce(pmlame0)
#' cbk.lame.delivery(pmlame1,intlame0)
cbk.lame.delivery <- function(pmlame,intlame,radius=12.5e-6,density=3,depth=30e-6,t=40){

  ##* Estimate sputter rate
  radius1      <- radius * 100                  # radius of a pit [cm]
  depth1       <- depth * 100                   # depth of a pit [cm]
  vol.pit      <- 3.141592 * radius1^2 * depth1 # volume sputtered [cc]
  num_a        <- 6.022e23                      # Avogadro constant [/mol]
  mass.pit     <- density * vol.pit

  ##* Rename rownames to be with "_" but "-"
  acqlist0          <- rownames(intlame)
  rownames(pmlame)  <- gsub("-","_",rownames(pmlame))
  rownames(intlame) <- gsub("-","_",rownames(intlame))

  ##* Setup
  isomeas      <- colnames(intlame)
  chemlist     <- cbk.iso(isomeas,'symbol')
  acqlist      <- rownames(intlame)
  stonelist    <- gsub("@[0-9]+$","",acqlist) # remove number after at mark

  ##* Generate pmlame with dimension that matches with that of intlame
  pmlame1      <- pmlame[stonelist,chemlist]

  ##* Have pseudo.atomic.weight with same dimensionas pmlame
  pseudo.wt    <- cbk.lame.rep(cbk.iso(isomeas,'pseudo.atomic.weight'),
                               length(stonelist),'v')

  ##* Real work
  yield <- intlame / (mass.pit / t * pmlame1 / pseudo.wt * num_a)

  ##* Restore rownames
  rownames(yield) <- acqlist0

  return(yield)
}
