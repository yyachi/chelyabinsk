#' Do mass balance calculation
#' @param datain Element abundances with row "WR"
#' @param element Vector of string such for c("Ba", "Nb", "La"...)
#' @param phase Vector of name of phases such for c("ol", "pig")
#' @param mode Vector of modal abundance of phases with label
#' @param density Vector of density of phases with label
#' @return Element abundances with rows of remainder and series of
#'     quantity, and columns of density and mode
#' @export
cas.balance <- function(datain,element,phase,mode,density){
  ### EXAMPLES
  ### > datain
  ###           Ba       Nb       La       Ce       Pr        Sr       Nd       Sm
  ### ol  0.044000 0.049000 0.000415 0.004941 0.000608  0.619333 0.000000 0.000000
  ### pig 0.059048 0.183133 0.003487 0.016731 0.004169  0.546383 0.041143 0.043506
  ### WR  4.510000 0.022500 0.013000 0.031000 0.004400 77.300000 0.029000 0.014000
  ###           Zr       Hf       Eu       Gd       Dy       Li        Y       Er
  ### ol  0.264633 0.024833 0.003022 0.004000 0.027036 1.635333 0.000000 0.023356
  ### pig 0.370700 0.043394 0.006900 0.069043 0.180000 0.682633 1.370666 0.178516
  ### WR  0.140000 0.005000 0.002800 0.032000 0.061000 2.050000 0.462000 0.057000
  ###           Yb       Lu
  ### ol  0.031766 0.010288
  ### pig 0.200000 0.037148
  ### WR  0.077000 0.014200
  ### > element
  ###  [1] "Ba" "Nb" "La" "Ce" "Pr" "Sr" "Nd" "Sm" "Zr" "Hf" "Eu" "Gd" "Dy" "Li" "Y"
  ### [16] "Er" "Yb" "Lu"
  ### > phase
  ### [1] "ol"  "pig"
  ### > mode
  ###    ol   pig    WR
  ### 0.525 0.353 1.000
  ### > density
  ###  ol pig  WR
  ### 3.3 3.4 3.3
  ### QQ <- cas.balance(datain,element,phase,mode,density)
  ### QQzCI <- cas.normalize(QQ,ref0)

  ### pre process
  phase_meas <- datain[phase,element]
  phase_meas[is.na(phase_meas)] <- 0  # replace NA by 0
  ## phase_mode    <- datain[phase,"mode"]
  phase_mode    <- mode[phase]
  ## phase_density <- datain[phase,"density"]
  phase_density <- density[phase]
  bulk_meas     <- datain["WR",element]
  bulk_mode     <- 1
  ## bulk_density  <- datain["WR","density"]
  bulk_density  <- density["WR"]

  ### element quantity of each phase considering mode and density
  for(jj in 1:length(phase_mode)) { # loop for phase
    jj_qtty           <- phase_meas[jj,] * phase_mode[jj] * phase_density[jj] / bulk_density
    rownames(jj_qtty) <- paste(rownames(jj_qtty),"(quantity)")
    if (jj == 1) {
      phase_qtty    <- jj_qtty
    } else {
      phase_qtty    <- rbind(phase_qtty,jj_qtty)
    }
  }

  ### essential calculation
  total_qtty            <- data.frame(t(colSums(phase_qtty))) # sum of element quantity
  rownames(total_qtty)  <- "WR (calc)"
  miss_mode             <- 1-sum(phase_mode) # missing phase (or remainder)
  miss_density          <- (bulk_density*bulk_mode - sum(phase_density*phase_mode))/miss_mode
  miss_qtty             <- bulk_meas - total_qtty
  rownames(miss_qtty)   <- "remainder (quantity)"
  miss_ab               <- miss_qtty/(miss_mode*miss_density/bulk_density)  # element abundance of the remainder
  miss_ab[miss_ab <= 0] <- NA
  rownames(miss_ab)     <- "remainder"

  ### after process
  phase_meas$mode       <- phase_mode
  phase_meas$density    <- phase_density
  phase_qtty$mode       <- phase_mode
  phase_qtty$density    <- phase_density
  bulk_meas$mode        <- bulk_mode
  bulk_meas$density     <- bulk_density
  total_qtty$mode       <- 1
  total_qtty$density    <- bulk_density
  miss_qtty$mode        <- miss_mode
  miss_qtty$density     <- miss_density
  miss_ab$mode          <- miss_mode
  miss_ab$density       <- miss_density

  return(rbind(phase_meas,bulk_meas,phase_qtty,total_qtty,miss_qtty,miss_ab))
}
