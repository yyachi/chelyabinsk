#' @title Rate delivery of ion from a pit by laser abbration
#' @description Rate delivery of ion from a pit by laser abbration.
#'   Although dimension of a pit is on meter [m], internaly, they are
#'   are converted into centi meter.
#' @param ion intensity of ion [cps]
#' @param iso name of ion (default: 'Si29')
#' @param ab chem abundance [g/g] (default: 0.23)
#' @param radius radius of a pit [m] (default: 12.5e-6)
#' @param density density of solid target [g/cc] (default: 3)
#' @param depth depth of a pit [m] (default: 30e-6)
#' @param t period of irradiation [s] (default: 40)
#' @return Rate of delivery
#' @export
#' @examples
#' ionml.rate.delivery(intensity=3.84e5)
ionml.rate.delivery <- function(intensity,
                                iso='Si29',
                                ab=0.23,
                                radius=12.5e-6,
                                density=3,
                                depth=30e-6,
                                t=40
                                ){

  radius1 <- radius * 100 # radius of a pit in [cm] instead of in [m]
  depth1  <- depth * 100 # depth of a pit in [cm] instead of in [m]

  Vpit    <- 3.141592 * radius1^2 * depth1 # mass sputtered by 1 acq [g]
  N_a     <- 6.022e23 # /mol Avogadro constant
  Mpit    <- density * Vpit

  transmission <- intensity /
    (Mpit / t * ab / cbk.iso(iso,'pseudo.atomic.weight') * N_a)
  message(sprintf("Delivery rate of %s is %g ppm\n", iso, transmission*1000000))

  return(transmission)
}
