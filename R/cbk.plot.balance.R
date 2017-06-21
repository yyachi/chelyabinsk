#' @title Calculate concentrations of WR and remainder from those of
#'   phases and mode of phases
#'
#' @description Calculate concentrations of WR and remainder from
#'   those of phases and mode of phases.
#'
#' @param pmlame_or_file A pmlame or CASTEML file with element
#'   abundance of phases without WR
#' @param reflame A dataframe with element abundance of WR
#' @param modeinfo A CSV file with rows of mode and density and
#'   columns of phases including WR
#' @param verbose Output debug info (default: FALSE).
#' @return A dataframe with rows of phases and columns mode and
#'   quantity of elements relative to WR
#' @export
#' @examples
#' reflame <- cbk.ref("Wasson.1988",property=cbk.periodic("atomicnumber"))
#' MM      <- cbk.plot.balance(cbk.path("demo-trc0.pml"),reflame,cbk.path("demo-mode0.csv"))
cbk.plot.balance <- function(pmlame_or_file,reflame,modeinfo,verbose=TRUE){
errorbar.y <- function(x,y,yerror,length=0.01,col=1,code=3){
  arrows(x, y, x, y+yerror, code=code, angle=90, length=length, col=col)
  arrows(x, y, x, y-yerror, code=code, angle=90, length=length, col=col)
}

## Load element abundance and mode
ZZ       <- cbk.read.casteml(pmlame_or_file)
DD       <- cbk.read.tblame(modeinfo)
## spec0 <- cbk.read.tblame('demo-spec0.csv')
ref0     <- cbk.lame.regulate(reflame,error=FALSE)

if (verbose) {
  cat(file=stderr(),"cbk.plot.balance:29: ZZ <-",cbk.lame.dump(ZZ,show=F),"\n")
  cat(file=stderr(),"cbk.plot.balance:30: DD <-",cbk.lame.dump(DD,show=F),"\n")
  cat(file=stderr(),"cbk.plot.balance:31: ref0 <-",cbk.lame.dump(ref0,show=F),"\n")
}

## ----------------------------------------
##* Config
## ----------------------------------------
## property0     <- cbk.periodic(property)               # "atomicnumber","volatility","compatibility"
## ref0          <- cbk.ref(reference,"none",property0)  # "Wasson.1988","Boynton.1989"
## phases        <- c("ol","opx","cpx","gl")             # Revise accordingly
phases        <- rownames(ZZ)

spec0         <- data.frame(row.names=c("WR","WR (calc)","remainder",phases)) # legend properties
spec0[,'col'] <- c("black","black","purple",2:(length(phases)+1))
spec0[,'pch'] <- c(NaN,NaN,17,1:length(phases))
spec0[,'lty'] <- c(1,2,2,rep(1,length(phases)))
spec0[,'lwd'] <- c(1,3,1,rep(1,length(phases)))

element       <- intersect(names(ref0),names(ZZ))
## XX         <- property0[element]
XX            <- 1:length(element)                       # distribute XX evenly
names(XX)     <- element                                 # distribute XX evenly

## Add CI as WR (June 19, 2017)
ZZ["WR",element] <- ref0[,element]


## ----------------------------------------
##* Process
## ----------------------------------------
phasianus        <- c(phases,"remainder")                       # list of phases including `missing phase'
density          <- cbk.vector(DD["density",])                  # note that density includes WR
mode             <- cbk.vector(DD["mode",])                     # note that mode includes WR

ZZzCI            <- cbk.lame.normalize(ZZ,ref0)                 # normalization to draw data in `[Z] over CI'
ZZzCI_mean       <- cbk.lame.regulate(ZZzCI,mean=T,error=F)
ZZzCI_error      <- cbk.lame.regulate(ZZzCI,mean=F,error=T)

QQ               <- cbk.balance(ZZ,element,phases,mode,density) # Estimate remainder in both concentration and quantity
QQzCI            <- cbk.lame.normalize(QQ,ref0)                 # normalization to CI to draw remainder in `[Z] over CI'

## ref8          <- as.data.frame(t(cbk.vector(ZZ["WR",element]))) # obtain WR-meas
## QQzWR         <- cbk.lame.normalize(QQ,ref8)                    # normalization to WR-meas for `Q over WR'
QQzWR            <- cbk.lame.normalize(QQ,ZZ["WR",element])     # normalization to WR-meas for `Q over WR'
QQzWR[QQzWR<0]   <- NaN

mode1            <- QQ[phasianus,"mode"]                        # relative mode of phase with remainder
names(mode1)     <- phasianus
MMQ              <- QQzWR[paste(phasianus,"(quantity)"),]       # relavie quantity of element
MM               <- cbind(mode=mode1, MMQ)
MM[MM<0]         <- 0
## MM[is.na(MM)] <- 0

## ----------------------------------------
##* Plot setup
## ----------------------------------------
par(mfrow=c(3,1),mar=c(2.5,4,0.1,0.1),mgp=c(3,1,0))

## ----------------------------------------
##* Concentration
## ----------------------------------------
## Make box
matplot(XX,t(ZZzCI_mean),type="n",log="y",axes=FALSE,xlab="",ylab="[Z] over CI",lwd=3)
axis(1,XX,labels=names(XX),cex.axis=1,las=2)
axis(2,cex.axis=1,axTicks(2),axTicks(2))
legend("bottomright",legend=rownames(spec0),cex=0.7,ncol=3,
       pch=spec0[,"pch"],col=spec0[,"col"],lty=spec0[,"lty"],lwd=spec0[,"lwd"])
## abline(h=(10^(-1:3)),lty="dashed") # grid
box()

## Draw phases and WR
phaselist <- rownames(ZZzCI_mean)
for(iphase in phaselist) { # ol, opx, cpx, gl, WR
  lines(XX,ZZzCI_mean[iphase,],type="o",
        pch=spec0[iphase,"pch"],col=spec0[iphase,"col"],lwd=spec0[iphase,"lwd"])
  errorbar.y(XX,as.numeric(ZZzCI_mean[iphase,]),as.numeric(ZZzCI_error[iphase,]),col=spec0[iphase,"col"])
}

## Draw WR (calc) and remainder
calclist <- c("WR (calc)","remainder")
for(icalc in calclist) { # WR (calc), remainder
  lines(XX,QQzCI[icalc,],type="o",
       pch=spec0[icalc,"pch"],col=spec0[icalc,"col"],lty=spec0[icalc,"lty"],lwd=spec0[icalc,"lwd"])
}


## ----------------------------------------
##* Quantity
## ----------------------------------------
## Make box
matplot(XX,t(QQzWR),type="n",log="y",axes=FALSE,
        xlab="",ylab="Q over WR",lwd=3)
axis(1,XX,labels=names(XX),cex.axis=1,las=2)
axis(2,cex.axis=1,axTicks(2),axTicks(2))
box() # bounding box

## Draw WR, WR (calc), remainder, and phases
quantlist  <- c("WR","WR (calc)","remainder (quantity)",paste(phases,"(quantity)"))
for(iquant in quantlist) { # WR, WR (calc), remainder (quantity), ol (quantity), opx (quantity), cpx (quantity), gl (quantity)
  myphase  <- gsub(" \\(quantity\\)","",iquant)
  lines(XX,QQzWR[iquant,],type="o",
        pch=spec0[myphase,"pch"],col=spec0[myphase,"col"],
        lwd=spec0[myphase,"lwd"],lty=spec0[myphase,"lty"])
}


## ----------------------------------------
##* Relative quantity
## ----------------------------------------
par(mgp=c(2.5,0.6,0))
barplot(as.matrix(MM),col=spec0[rownames(MM),'col'],las=2)

## ----------------------------------------
##* Closing remarks
## ----------------------------------------
  return(MM)
}
