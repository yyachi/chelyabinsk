#' Replicate pmlame in horizontal or vertical direction
#' @param pmlame A pmlame to be replicated
#' @param times Number of replication
#' @param direction Direction to make replicate (`h' or `v').  This
#'   function guesses direction to be replicated when pmlame is single
#'   row or column.
#' @return A pmlame with repeated column or row.
#' @export
#' @examples
#' pmlame0 <- data.frame(row.names=c("stone.1","stone.2"), SiO2=c(0.525,0.420))
#' cbk.lame.rep(pmlame0,3)
cbk.lame.rep <- function(pmlame,times,direction=NULL) {

  # Guess direction
  if(is.null(direction)){
    if (ncol(pmlame) == 1) {
      direction = 'h'
    } else if (nrow(pmlame) == 1){
      direction = 'v'
    }
  }

  # Accept not only `h' and `v'
  if (direction %in% c('c','col','column','h','horz')) {
    direction <- 'h'
  }
  if (direction %in% c('r','row','v','vert')) {
    direction <- 'v'
  }

  # Real work
  if (direction == 'h') {
    pmlame <- do.call(cbind,replicate(times,pmlame,simplify=FALSE))
  } else {
    pmlame <- do.call(rbind,replicate(times,pmlame,simplify=FALSE))
  }
  return(pmlame)
}
