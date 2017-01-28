#' @title Rate delivery of ion from a pit by laser abbration
#' @description Rate delivery of ion from a pit by laser abbration.
#'   Although dimension of a pit is on meter [m], internaly, they are
#'   are converted into centi meter.
#' @param pmlame A pmlame with row of stone and column of chem
#' @param intlame A pseudo-pmlame of ion intensity with row of stone
#'   and column of isomeas [cps]
#' @param radius radius of a pit [m] (default: 12.5e-6)
#' @param density density of solid target [g/cc] (default: 3)
#' @param depth depth of a pit [m] (default: 30e-6)
#' @param t period of irradiation [s] (default: 40)
#' @return Yield from solid target to detector
#' @export
#' @examples
ionml.rate.delivery <- function(pmlame,intlame,radius=12.5e-6,density=3,depth=30e-6,t=40){

  ## Estimate sputter rate
  radius1      <- radius * 100                  # radius of a pit [cm]
  depth1       <- depth * 100                   # depth of a pit [cm]
  vol.pit      <- 3.141592 * radius1^2 * depth1 # volume sputtered [cc]
  num_a        <- 6.022e23                      # Avogadro constant [/mol]
  mass.pit     <- density * vol.pit

  ## Setup
  isomeas      <- colnames(intlame)
  stonelist    <- rownames(intlame)

  ## Extract element abundance
  pmlame1      <- pmlame[stonelist,cbk.iso(isomeas,'symbol')]
  
  ## Have pseudo.atomic.weight with elements as same as pmlame
  pseudo.wt    <- cbk.lame.rep(cbk.iso(isomeas,'pseudo.atomic.weight'),
                               length(stonelist),'v')

  ## Read work
  transmission <- intlame / (mass.pit / t * pmlame1 / pseudo.wt * num_a)
  ## message(sprintf("Delivery rate of %s is %g ppm\n", isomeas, transmission*1000000))

  return(transmission)
}
