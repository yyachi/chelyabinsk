#' @title Estimate reproducibility of acquisition by each stone
#' @description Estimate reproducibility of acquisition by each stone.
#'   Average and standard deviation are calculated from mean value of
#'   pmlame, and pmlame with mean and error will be created.
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param stonefy_regexp A regexp string to groupfy stones.
#' @param verbose Output debug info (default: FALSE).
#' @return A pmlame of mean and error values.
#' @seealso \code{cbk.lame.colMeans} and \code{cbk.lame.colSds}
#' @export
#' @examples
#' pmlame <- structure(list(Li7 = c(0.000506512438134679, 0.000516486780072755, 0.000507547487581988, 0.000595083203981413, 0.000615809376597437, 0.00149434864755115, 0.00143122177897333, 0.00154206737152817, 0.00169517782754287, 0.00159391603394293, 0.00116657932013985, 0.00118695452022804, 0.00113829905991248, 0.00133416408491479, 0.0011442210539973), Si29 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), Sr88 = c(0.0392195949852386, 0.0390280091434152, 0.0391113998900179, 0.0399550930587581, 0.0391245435379003, 0.0936516349778358, 0.0939928951250021, 0.091981285014914, 0.089583007172954, 0.0932933026080165, 0.0743836387584856, 0.0740646289934491, 0.0706444462225087, 0.0742444965997019, 0.0736867415618733), Y89 = c(0.0146955297660687, 0.0144090670460939, 0.0140735744070246, 0.0147961091181929, 0.0141715091125382, 0.00441697935485345, 0.00434730579593007, 0.00443420320996267, 0.00449693854025482, 0.00459303138997202, 0.00470557103590392, 0.00456946684203066, 0.0044608267480778, 0.00459763846146595, 0.00461070415803766)), .Names = c("Li7", "Si29", "Sr88", "Y89"), row.names = c("ref_cpx_klb1@25", "ref_cpx_klb1@26", "ref_cpx_klb1@27", "ref_cpx_klb1@28", "ref_cpx_klb1@29", "ref_cpx_sax33@25", "ref_cpx_sax33@26", "ref_cpx_sax33@27", "ref_cpx_sax33@28", "ref_cpx_sax33@29", "ref_cpx_sax39c@25", "ref_cpx_sax39c@26", "ref_cpx_sax39c@27", "ref_cpx_sax39c@28", "ref_cpx_sax39c@29"), class = "data.frame")
#' cbk.lame.stat(pmlame)
cbk.lame.stat <- function(pmlame,stonefy_regexp="@[[:digit:]]+$",verbose=FALSE){
  meanlame0 <- cbk.lame.regulate(pmlame,mean=T,error=F,extra=F)
  meanlame  <- cbk.lame.colMeans(meanlame0,stonefy_regexp=stonefy_regexp)
  errorlame <- cbk.lame.colSds(meanlame0,stonefy_regexp=stonefy_regexp)
  pmlame1   <- cbk.lame.merge.error(meanlame,errorlame)
  
  return(pmlame1)
}
