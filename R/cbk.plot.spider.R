#' @title Read CASTEML file and create spider diagram
#'
#' @description Read CASTEML dataframe and create spider diagram.
#'   This function does not save the created diagram.  You should
#'   prepare a canvas in advance.
#'
#' @param pmlfile_or_stone A CASTEML file that exits locally or
#'   stone-ID (or pmlame).
#' @param tableunit Unit to toss to \link{cbk.read.casteml}.
#' @param property Property to align x-axis
#' @param reference Reference of element abundance
#' @param opts List of further options for plot.  See
#'   \code{\link{cbk.plot}}.
#' @return A pmlmae used to plot the diagram.
#' @export
#' @seealso \url{https://github.com/misasa/casteml}
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' pmlame  <- cbk.read.casteml(pmlfile,tableunit="ug/g",category=NULL)
#' cbk.plot.spider(pmlame)
cbk.plot.spider <- function(pmlfile_or_stone,opts=NULL,tableunit="ug/g",property="atomicnumber",reference="Wasson.1988") {
  opts_default <- list(legendp=TRUE, Recursivep=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)

  ### ----------------
  ###* OPENING REMARK
  ### ----------------
  ## pmlame <- cbk.read.casteml(pmlfile,tableunit,category=NULL)
  pmlame    <- cbk.read.casteml(pmlfile_or_stone,opts,tableunit)
  periodic  <- cbk.periodic()
  ref1      <- cbk.ref(reference,tableunit,cbk.periodic(property))
  ###
  ### extract "Si" and element numbers
  ##   oxidelist        <- c("SiO2", "Al2O3", "CaO", "MgO", "Fe2O3", "FeO", "Na2O", "H2O+", "TiO2", "K2O", "P2O5", "MnO")
  ##   convector        <- c("Si"  , "Al"   , "Ca" , "Mg" , "Fe"   , "Fe" , "Na"  , "H"   , "Ti"  , "K"  , "P"   , "MnO")
  ##   names(convector) <- oxidelist
  ##   convector        <- rbind(convector,c(  1,    2,    1,    1,    2,    1,    2,   2,    1,   2,   2,     1))
  ##   convector        <- rbind(convector,c(  2,    3,    1,    1,    3,    1,    1,   1,    2,   1,   5,     1))

  ## ### add "Si" column to pmlame
  ##   oxygen <- 15.9994
  ##   for(ii in 1:length(oxidelist)) {
  ##     obj         <- convector[,oxidelist[ii]][1]
  ##     objnum      <- as.numeric(convector[,oxidelist[ii]][2])
  ##     oxynum      <- as.numeric(convector[,oxidelist[ii]][3])

  ##     objmass     <- periodic[obj,"mass"]
  ##     oxideweight <- objmass * objnum + oxygen * oxynum
  ##     pmlame[,obj]  <- pmlame[,oxidelist[ii]] * objmass * objnum / oxideweight
  ##   }
  pmlame1  <- cbk.lame.reduce(pmlame)
  ###
  ###
  ## stonelist   <- rownames(pmlame1)
  ## stoneindex  <- 1:nrow(pmlame1)
  chemlist <- colnames(pmlame1)

  ## ----------------
  ##* PARSE
  ## ----------------
  property0        <- periodic[chemlist,property] # atomicnumber, volatility, compatibility
  names(property0) <- chemlist
  XX0              <- sort(property0)
  XX               <- 1:length(XX0)
  ZZ0              <- pmlame1[,names(XX0),drop=FALSE]
  ZZ               <- cbk.lame.drop.dharma(ZZ0)
  CI               <- cbk.vector(ref1[names(XX0)])
  YY               <- t(ZZ) / CI
  ## ind              <- apply(YY, 2, function(x) all(is.na(x)))
  ## YY               <- YY[ ,!ind ]
  stonelist        <- colnames(YY)
  stoneindex       <- 1:ncol(YY)

  ## ----------------
  ##* PLOTS
  ## ----------------
  ## par(mar=c(4.5,4.5,0.5,0.5),mfrow=c(2,1)) # c(bottom,left,top,right) c(5.1,4.1,4.1,2.1)

  matplot(XX,YY,log="y",type="o",lty=1,pch=stoneindex,
          xlab='',ylab='ZZ/CI',axes=FALSE)
  axis(1,at=XX,labels=names(XX0),cex.axis=0.9,las=2)
  axis(2,axTicks(2),axTicks(2))
  abline(h=1,lty=2)
  box(lwd=1)
  if (opts$legendp) {
    legend('bottomright',stonelist,lty=1,pch=stoneindex,col=stoneindex,ncol=4,cex=0.5)
  }
  ### ----------------
  ###* CLOSING REMARK
  ### ----------------
  return(pmlame1)
}
