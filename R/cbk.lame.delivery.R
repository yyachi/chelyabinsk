#' @title Estimate delivery rate of element from sputter rate
#' @description Estimate delivery rate of element from sputter rate.
#'   Calculation is made in dimension of CGS.
#' @details With ion Si29 intensity 3.85e+05 [cps], element SiO2
#'   abundance 59 wt%, and sputter rate 1.1 ng/s, the delivery rate is
#'   estimated to be 1.25 ppm.
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param intlame A pseudo-pmlame of ion intensity with row of stone
#'   and column of isomeas [cps].  Typically rowname is with
#'   `@acq-number'.
#' @param sputter.rate Sputter rate [g/s] (default: 1.1e-9 that
#'   corresponds to abbration of 40 s to produce a pit with diameter
#'   of 25 micron, depth of 30 micron, and density of 3 g/cc).
#' @param verbose Output debug info (default: FALSE).
#' @return Delivery rate of element from solid target to detector.
#' @export
#' @examples
#' pmlame0      <- data.frame(row.names=c("ref-gl-tahiti","ref-cpx-klb1"),SiO2=c(0.590,0.520),Li=c(20.8e-06,1.37e-06),Sr=c(107e-06,36.1e-06))
#' pmlame1      <- cbk.lame.reduce(pmlame0)
#' intlame0     <- data.frame(row.names=c("ref_gl_tahiti@1","ref_cpx_klb1@1"),Si29=c(3.85e+05,3.95e+05),Li7=c(1.87e+03,1.73e+02),Sr88=c(3.24e+04,1.50e+04))
#' radius       <- 12.5e-6 # Radius of a pit [m]
#' depth        <- 30e-6   # Depth of a pit [m]
#' t            <- 40      # Period of irradiation [s]
#' density      <- 3       # Density of solid target [g/cc]
#' sputter.rate <- 3.14 * (radius * 100)^2 * (depth * 100) * density / t # [g/s]
#' cbk.lame.delivery(pmlame1,intlame0,sputter.rate)
cbk.lame.delivery <- function(pmlame,intlame,sputter.rate=1.1e-09,verbose=FALSE){
  ##* Constant
  isomeas_regexp      <- "^[A-Z][a-z]?[0-9]+$"
  ## stonelist_regexp <- "@[0-9]+$"        # ref_gl_tahiti@10um@5Hz@x200(@11)
  stonelist_regexp    <- "@[@[:alnum:]]+$" # ref_gl_tahiti(@10um@5Hz@x200@11)

  num_a               <- 6.022e23 # Avogadro number [/mol]
  acqlist0            <- rownames(intlame)

  ##* Rename rownames to be with "_" but "-" within this function
  row.names(pmlame)   <- gsub("-","_",row.names(pmlame))
  row.names(intlame)  <- gsub("-","_",row.names(intlame))

  ##* Setup
  isomeas             <- grep(isomeas_regexp,colnames(intlame),value=T)
  chemlist            <- cbk.iso(isomeas,'symbol')
  acqlist             <- rownames(intlame)
  stonelist           <- gsub(stonelist_regexp,"",acqlist) # remove number after at mark

  ##* Console
  if (verbose) {
    cat(file=stderr(),"cbk.lame.delivery:45: isomeas # =>",isomeas,"\n")
    cat(file=stderr(),"cbk.lame.delivery:46: chemlist # =>",chemlist,"\n")
    cat(file=stderr(),"cbk.lame.delivery:47: acqlist # =>",acqlist,"\n")
    cat(file=stderr(),"cbk.lame.delivery:48: stonelist # =>",stonelist,"\n")
    cat(file=stderr(),"cbk.lame.delivery:49: rownames(intlame) # =>",rownames(intlame),"\n")
    cat(file=stderr(),"cbk.lame.delivery:50: colnames(intlame) # =>",colnames(intlame),"\n")
  }

  ##* Have intlame, pmlame, and pseudo.t with the same dimension
  intlame1          <- intlame[acqlist,isomeas,drop=FALSE]
  pmlame1           <- pmlame[stonelist,chemlist,drop=FALSE]
  rownames(pmlame1) <- acqlist
  pseudo.wt         <- cbk.lame.rep(cbk.iso(isomeas,'pseudo.atomic.weight'),length(stonelist),'v')

  ##* Console
  if (verbose) {
    cat(file=stderr(),"cbk.lame.delivery:61: intlame1 <-",cbk.lame.dump(intlame1,show=F),"\n")
    cat(file=stderr(),"cbk.lame.delivery:62: pmlame <-",cbk.lame.dump(pmlame,show=F),"\n")
    cat(file=stderr(),"cbk.lame.delivery:63: pmlame1 <-",cbk.lame.dump(pmlame1,show=F),"\n")
    cat(file=stderr(),"cbk.lame.delivery:64: pseudo.wt <-",cbk.lame.dump(pseudo.wt,show=F),"\n")
  }

  ##* Real work
  yield             <- intlame1 / (sputter.rate * pmlame1 / pseudo.wt * num_a)

  ##* Restore rownames
  rownames(yield)   <- acqlist0

  return(yield)
}
