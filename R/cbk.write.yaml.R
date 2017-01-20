#' Write a pmlame to yamlfile.
#'
#' @param pmlame A pmlame with rows of stone and columns of chem.
#' @param yamlfile A yamlfile with a pmlame.
#' @param verbose Output debug info (default: TRUE).
#' @return A pmlame with columns of chem and rows of stone.
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
    cat(file=stderr(),"cbk.write.yaml:20: yamlfile # =>",yamlfile,"\n")
  }
  
  write(yaml::as.yaml(pmlame1),file=yamlfile)

  return(yamlfile)
}
