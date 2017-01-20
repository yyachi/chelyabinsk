#' Read a pmlame stored in yamlfile.
#'
#' @param yamlfile A yamlfile with pmlame, that is with columns of
#'   chem and rows of stone.
#' @param verbose Output debug info (default: TRUE).
#' @return A pmlame with columns of chem and rows of stone.
#' @seealso \code{\link{cbk.write.yaml}},
#'   \code{\link{cbk.read.casteml}}.
#' @export
cbk.read.yaml <- function(yamlfile,verbose=TRUE){

  if (verbose) {
    cat(file=stderr(),"cbk.read.yaml:14: yamlfile # =>",yamlfile,"\n")
  }

  pmlame0            <- data.frame(yaml::yaml.load_file(yamlfile))
  row.names(pmlame0) <- pmlame0$rownames
  pmlame             <- pmlame0[,colnames(pmlame0)!='rownames']

  return(pmlame)
}
