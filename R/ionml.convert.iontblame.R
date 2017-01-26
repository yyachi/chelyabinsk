#' @title Convert ion-type TBLAME.csv to IONML
#'
#' @description Convert ion-type TBLAME.csv to IONML.
#' 
#'   This really create complete IONML.  This program assumes
#'   extensions of ion-type TBLAME.csv and IONML to be `.ion' and
#'   `.xml', respectively.
#'
#' @param iontblame Name of ion-type TBLAME.csv.
#' @param outfile Name of IONML that will be created.
#' @param force Flag to force convert again.
#' @return Name of IONML that was created.
#' @seealso \code{ionml.convert.laicpqms}
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
    ## pmlame0    <- pmlame0[c(1,2,3,4,5),] # to make data less

    chemlist   <- colnames(pmlame0)
    chemlist   <- chemlist[-which(chemlist %in% "time")]
    ## chemlist   <- colnames(pmlame0[,colnames(pmlame0)!="time"])
    ncycle     <- length(pmlame0[,"time"])

    ## Output to XML
    ## http://stackoverflow.com/questions/6256064/how-to-create-xml-from-r-objects-e-g-is-there-a-listtoxml-function

    top <- newXMLNode("ion_link")
    doc <- newXMLDoc(node=top)

    # title
    title      <- newXMLNode("title", "Analysis Data Report", parent = top)
    # time stamp
    time_stamp <- newXMLNode("time_stamp", parent = top)
    xts_data   <- newXMLNode("data",  format(Sys.time(), "%m/%d/%Y %b %X"), parent = time_stamp)
    xts_info   <- newXMLNode("info",  parent = time_stamp)
    xts_unit   <- newXMLNode("unit",  parent = time_stamp)
    xts_label  <- newXMLNode("label", parent = time_stamp)

    ## each ion...
    for(ichem in chemlist) {
      # for compatibility, name should be int_151Eu instead of Eu151
      isoname      <- gsub("([A-Z][a-z]?)([0-9]+)","int_\\2\\1",ichem) # Eu151 -> 151Eu

      # target
      target       <- newXMLNode("target", parent = top)
      ## name
      xt_name      <- newXMLNode("name", isoname, parent = target) # "int_7Li"
      ## unit
      xt_unit      <- newXMLNode("unit", "cps", parent = target)
      xt_unit_time <- newXMLNode("time", "sec", parent = xt_unit)
      ## data...
      for(jj in 1:ncycle) {
        xt_data    <- newXMLNode("data", pmlame0[jj,ichem], attrs = c(time = pmlame0[jj,"time"]), parent = target)
      }
    }

    outfile <- saveXML(doc,outfile,encoding="UTF-8") # indent=FALSE
  }
  return(outfile)
}
