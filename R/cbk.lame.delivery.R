#' @title Estimate delivery rate of chem
#' @description Estimate delivery rate of chem.  Calculation is made
#'   in dimension of CGS.
#' @details With ion Si29 intensity 3.85e+05 [cps], element SiO2
#'   abundance 59 wt%, and sputter rate 1.1 ng/s, delivery rate is
#'   estimated to be 1.25 ppm.
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param intlame A pseudo-pmlame of ion intensity with row of stone
#'   and column of isomeas [cps].  Typically rowname is with
#'   `@acq-number'.
#' @param sputter.rate Sputter rate [g/s] (default: 1.1e-9 that
#'   corresponds to abbration of 40 s to produce a pit with diameter
#'   of 25 micron, depth of 30 micron, and density of 3 g/cc).  This
#'   also can be vector with name of acqlist, that should be
#'   consistent with rownames of intlame.
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
  if (verbose) {
    cat(file=stderr(),"cbk.lame.delivery:30: names(sputter.rate) # =>",names(sputter.rate),"\n")
  }

  ##* Constant
  ## isomeas_regexp <- "^[A-Z][a-z]?[0-9]+$"
  ## stonify_regexp <- "@[0-9]+$"        # ref_gl_tahiti@10um@5Hz@x200(@11)
  stonify_regexp    <- "@[@[:alnum:]]+$" # ref_gl_tahiti(@10um@5Hz@x200@11)

  num_a             <- 6.022e23 # Avogadro number [/mol]
  acqlist0          <- rownames(intlame)

  ##* Rename all rownames to be with "_" but "-" within this function
  rownames(pmlame)  <- gsub("-","_",rownames(pmlame))
  rownames(intlame) <- gsub("-","_",rownames(intlame))

  ##* Check names of vector
  if (length(sputter.rate) > 1) {
    if (is.null(names(sputter.rate))) {
      cat(file=stderr(),"Warning: cbk.lame.delivery:47: Consider giving names to sputter.rate\n")
    } else {
      names(sputter.rate) <- gsub("-","_",names(sputter.rate)) # rename to be "_" within this function
      if (!all(sort(names(sputter.rate)) == sort(rownames(intlame)))) {
        cat(file=stderr(),"cbk.lame.delivery:51: names(sputter.rate) # =>",names(sputter.rate),"\n")
        cat(file=stderr(),"cbk.lame.delivery:52: rownames(intlame) # =>",rownames(intlame),"\n")
        stop("Inconsistent of rownames(intlame) and names(sputter.rate)")
      }
      sputter.rate <- sputter.rate[rownames(intlame)]
    }
  }

  ##* Setup
  acqlist    <- rownames(intlame)
  stonelist  <- gsub(stonify_regexp,"",acqlist) # remove letters after the first `at mark'
  ## isomeas <- grep(isomeas_regexp,colnames(intlame),value=T)
  isomeas    <- colnames(cbk.lame.regulate(intlame,mean=T,error=F,extra=F))
  chemlist   <- cbk.iso(isomeas,'symbol')

  ##* Console
  if (verbose) {
    cat(file=stderr(),"cbk.lame.delivery:66: acqlist # =>",acqlist,"\n")
    cat(file=stderr(),"cbk.lame.delivery:67: stonelist # =>",stonelist,"\n")
    cat(file=stderr(),"cbk.lame.delivery:68: isomeas # =>",isomeas,"\n")
    cat(file=stderr(),"cbk.lame.delivery:69: chemlist # =>",chemlist,"\n")
    cat(file=stderr(),"cbk.lame.delivery:70: rownames(intlame) # =>",rownames(intlame),"\n")
    cat(file=stderr(),"cbk.lame.delivery:71: colnames(intlame) # =>",colnames(intlame),"\n")
  }

  ##* Have intlame, pmlame, and pseudo.t with the same dimension
  intlame1            <- intlame[,isomeas,drop=FALSE]
  pmlame1             <- pmlame[stonelist,chemlist,drop=FALSE]
  rownames(pmlame1)   <- acqlist
  pseudo.wt           <- cbk.lame.rep(cbk.iso(isomeas,'pseudo.atomic.weight'),length(stonelist),'v')
  rownames(pseudo.wt) <- acqlist

  ##* Console
  if (verbose) {
    cat(file=stderr(),"cbk.lame.delivery:89: intlame1 <-",cbk.lame.dump(intlame1,show=F),"\n")
    cat(file=stderr(),"cbk.lame.delivery:90: sputter.rate <-",cbk.lame.dump(sputter.rate,show=F),"\n")
    cat(file=stderr(),"cbk.lame.delivery:91: pmlame1 <-",cbk.lame.dump(pmlame1,show=F),"\n")
    cat(file=stderr(),"cbk.lame.delivery:92: pseudo.wt <-",cbk.lame.dump(pseudo.wt,show=F),"\n")
    cat(file=stderr(),"cbk.lame.delivery:93: num_a <-",num_a,"\n")
  }

  ##* Real work
  yield             <- intlame1 / (sputter.rate * pmlame1 / pseudo.wt * num_a)

  ##* Restore rownames back
  rownames(yield)   <- acqlist0 # Restore including "-" or "_"

  return(yield)
}
