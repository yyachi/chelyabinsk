#' Write a pmlame to yamlfile.
#'
#' @details This function uses \code{yaml::as.yaml} to save pmlame.
#'   Since \code{yaml::as.yaml} drops rownames, this function stores
#'   rownames using a column with label `rownames'.
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param yamlfile Path to yamlfile with a pmlame.
#' @param verbose Output debug info (default: TRUE).
#' @return Path to yamlfile.
#' @seealso \code{\link{cbk.read.yaml}} and
#'   \code{\link{cbk.write.casteml}}.
#' @export
cbk.write.yaml <- function(pmlame,yamlfile=NULL,verbose=TRUE){

  pmlame1          <- pmlame
  pmlame1$rownames <- rownames(pmlame)

  if(is.null(yamlfile)){
    yamlfile <- file.path(tempdir(),paste0(digest::digest(pmlame,algo='md5'),".yaml"))
  }

  if (verbose) {
    cat(file=stderr(),"cbk.write.yaml:23: yamlfile # =>",yamlfile,"\n")
  }

  write(yaml::as.yaml(pmlame1),file=yamlfile)

  return(yamlfile)
}
