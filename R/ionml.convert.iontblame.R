#' @title Convert ion-type TBLAME.CSV to IONML
#'
#' @description Convert ion-type TBLAME.CSV to IONML.
#' 
#'   This really create complate IONML.  This program assumes
#'   extensions of ion-type TBLAME.CSV and IONML to be `.ion' and
#'   `.xml', respectively.
#'
#' @param iontblame Name of ion-type TBLAME.CSV
#' @param outfile Name of IONML that will be created
#' @param force Flag to force convert again
#' @return Name of IONML that was created
#' @seealso ionml.convert.laicpqms
#' @export
#' @examples
#' iontblame <- cbk.path("ref_cpx_klb1@1.ion")
#' ionml.convert.iontblame(iontblame,outfile=tempfile(fileext=".xml"))
ionml.convert.iontblame <- function(iontblame,outfile=NULL,force=FALSE) {
  library(XML) # install.packages('XML')
  
  ## Guess extension of iontblame on omit
  if (grepl("",tools::file_ext(iontblame))) {
    iontblame <- paste0(tools::file_path_sans_ext(iontblame),".ion")
  }

  cat(file=stderr(),"ionml.convert.iontblame:25: iontblame # =>",iontblame,"\n")

  ## Set name of outfile by default
  if(is.null(outfile)){
    outfile <- paste0(tools::file_path_sans_ext(iontblame),".xml")
  }

  ## Convert file only when destination does not exist
  if (force || !file.exists(outfile)) {

    ## Input
    pmlame0    <- cbk.read.tblame(iontblame)
    ## pmlame0    <- pmlame0[c(1,2,3,4),] # to make data less

    chemlist   <- colnames(pmlame0[,colnames(pmlame0)!="time"])
    ncycle     <- length(pmlame0[,"time"])

    ## Output to XML
    ## http://stackoverflow.com/questions/6256064/how-to-create-xml-from-r-objects-e-g-is-there-a-listtoxml-function

    top <- newXMLNode("ion_link")
    doc <- newXMLDoc(node=top)

    # title
    title      <- newXMLNode("title", "iCAP-Q with laser Analysis Data Report", parent = top)
    # time stamp
    time_stamp <- newXMLNode("time_stamp", parent = top)
    ts.data    <- newXMLNode("data",  format(Sys.time(), "%a %b %d %X %Y"), parent = time_stamp)
    ts.info    <- newXMLNode("info",  parent = time_stamp)
    ts.unit    <- newXMLNode("unit",  parent = time_stamp)
    ts.label   <- newXMLNode("label", parent = time_stamp)

    ## each ion...
    for(ichem in chemlist) {
      # for compatibility, name should be int_151Eu instead of Eu151
      ichem <- gsub("([A-Z][a-z]?)([0-9]+)","int_\\2\\1",ichem) # Eu151 -> 151Eu

      # target
      itarget    <- newXMLNode("target", parent = top)
      ## name
      iname      <- newXMLNode("name", ichem, parent = itarget) # "int_7Li"
      ## unit
      iunit      <- newXMLNode("unit", "cps", parent = itarget)
      iunit.time <- newXMLNode("time", "sec", parent = iunit)
      ## data...
      for(jj in 1:ncycle) {
        ichem.jdata <- newXMLNode("data", pmlame0[jj,ichem], attrs = c(time = pmlame0[jj,"time"]), parent = itarget)
      }
    }

    outfile <- saveXML(doc,outfile)
  }
  return(outfile)
}
