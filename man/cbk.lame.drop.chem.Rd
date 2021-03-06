% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbk.lame.drop.chem.R
\name{cbk.lame.drop.chem}
\alias{cbk.lame.drop.chem}
\title{Drop chem from pmlame}
\usage{
cbk.lame.drop.chem(pmlame, chem, verbose = FALSE)
}
\arguments{
\item{pmlame}{A pmlame with row of stone and column of chem [g/g].}

\item{chem}{List of chem to drop from pmlame}

\item{verbose}{Output debug info (default: FALSE).}
}
\value{
A pmlame without the specified chem
}
\description{
Drop chem from pmlame
}
\examples{
pmlame <- data.frame(row.names=c("stone@1","stone@2","stone@3"), SiO2=c(0.525,0.420,0.332), fo=c(0.353,3.400,20.323), d7Li=c(1.0,3.3,-1.0))
cbk.lame.drop.chem(pmlame,"fo")
cbk.lame.drop.chem(pmlame,c("fo","d7Li"))
}
\seealso{
\code{cbk.lame.drop.chem}
}
