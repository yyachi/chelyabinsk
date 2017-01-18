#' @title Read CASTEML file and create spots diagram
#'
#' @description Read CASTEML dataframe and create spots diagram.
#'   This function does not save the created diagram.  You should
#'   prepare a canvas in advance.
#'
#' @param pmlfile_or_surface A CASTEML file that exits locally or
#'   stone-ID (or pmlame).
#' @param imagefile Image shown on background of analyzed spots.
#' @param opts List of further options for plot.  See
#'   \code{\link{cbk.plot}}.
#' @return A pmlmae used to plot the diagram.
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.download.casteml("20160819165624-372633")
#' cbk.plot.spots(pmlfile)
cbk.plot.spots <- function(pmlfile_or_surface,opts=NULL,imagefile=NULL) {
  library(jpeg) # install.packages('jpeg')
  library(png) # install.packages('png')
  opts_default <- list(legendp=TRUE, Recursivep=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)
  ## ----------------
  ##* OPENING REMARK
  ## ----------------
  ## pmlame <- cbk.read.casteml(pmlfile,tableunit,category="oxygen")
  pmlame0   <- cbk.read.casteml(pmlfile_or_surface,opts)
  pmlame1   <- pmlame0[,c("x_image","y_image")]
  pmlame1   <- cbk.lame.drop.dharma(pmlame1)

  stonelist  <- rownames(pmlame1)
  stoneindex <- 1:nrow(pmlame1)

  ## ----------------
  ##* PARSE
  ## ----------------
  XX   <- pmlame1[,'x_image']
  YY   <- pmlame1[,'y_image']

  ## ----------------
  ##* PLOT
  ## ----------------
  if (is.null(imagefile)) {
    imagefile=try(cbk.download.image(pmlfile_or_surface))
  }
  if (class(imagefile) != "try-error"){
    ext <- tools::file_ext(imagefile)
    if (grepl(tolower(ext),"png")){
      img <- readPNG(imagefile)
    } else if (grepl(tolower(ext),"jpg")) {
      img <- readJPEG(imagefile)
    } else {
      stop("Such image type is not supported")
    }
    plot(XX,YY,type="n",axes=FALSE,asp=1,
         xlim=c(-50,50), ylim=c(-50,50),
         xlab="",ylab="")
    if (dim(img)[1] > dim(img)[2]) {  # for portrait image
      xmax <- 50 * dim(img)[2] / dim(img)[1]
      ymax <- 50
    } else {                          # for landscape image
      xmax <- 50
      ymax <- 50 * dim(img)[1] / dim(img)[2]
    }
    rasterImage(img, -xmax, -ymax, xmax, ymax) # (image, xleft, ybottom, xright, ytop)
    par(new=T)
  }
  plot(XX,YY,type="p",asp=1,
       xlim=c(-50,50), ylim=c(-50,50),
       pch=stoneindex,col=stoneindex,
       xlab="",ylab="")

  if (opts$legendp) {
    legend('bottomright',stonelist,ncol=4,cex=0.5,pch=stoneindex,col=stoneindex)
  }

  ## ----------------
  ##* CLOSING REMARK
  ## ----------------
  return(pmlame1)
}
