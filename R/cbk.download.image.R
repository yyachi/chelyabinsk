#' @title Download imagefile from a surface
#'
#' @description Download imagefile from a surface. This function
#'   download CASTEML of a surface, parse them and extract attachment
#'   tag in the first spot, and download to local directory. When
#'   outfile exists, this function does not download a new imagefile.
#' 
#' @param pmlfile_or_surface Unique ID of surface. Really, surface can
#'   be pmlfile, stone, and bib although only the first image will be
#'   downloaded.
#' @param outfile Path to save a imagefile. Unless specified,
#'   imagename of Medusa will be located on current directory.
#' @param force Download again and overwrite preexisting local
#'   imagefile.
#' @return Path to the imagefile downloaded
#' @export
#' @seealso \code{casteml download},
#'   \url{https://github.com/misasa/casteml}, and
#'   \code{\link{cbk.convert.casteml}}
#' @examples
#' pmlfile <- cbk.download.casteml("20160819165624-372633")
#' imagefile <- cbk.download.image(pmlfile)
#'
#' imagefile <- cbk.download.image("20160819165624-372633")
cbk.download.image <- function(pmlfile_or_surface,outfile=NULL,force=FALSE) {
  library(yaml)
  library(urltools)
  library(XML)

  if (file.exists(pmlfile_or_surface)) { # existing-pmlfile fed
    pmlfile <- pmlfile_or_surface
  } else {                             # stone fed
    surface <- pmlfile_or_surface
    ## if (opts$Recursivep) {
    ##   pmlfile  <- cbk.download.casteml(c("-R", surface))
    ## } else {
    pmlfile <- cbk.download.casteml(c("-r", surface))
    ## }
  }
  cat(file=stderr(),"cbk.download.image:40: pmlfile # =>",pmlfile,"\n")
  doc           <- xmlParse(pmlfile)
  nodes         <- getNodeSet(doc, "//acquisition/spot")
  spots         <- lapply(nodes, function(x) xmlToList(x))
  file_path     <- spots[[1]]$attachment_file_path
  orochirc      <- yaml.load_file("~/.orochirc")
  file_url      <- paste("http://",domain(orochirc$uri),file_path, sep="")
  if (is.null(outfile)) {
    file_basename <- strsplit(basename(file_path),'[.?]')[[1]][1]
    file_ext      <- strsplit(basename(file_path),'[.?]')[[1]][2]
    outfile       <- paste(file_basename, file_ext,sep=".")
  }
  if (force || !file.exists(outfile)) {
    download.file(file_url, outfile)
  }
  return(outfile)
}
