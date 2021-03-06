% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbk.lame.reduce.R
\name{cbk.lame.reduce}
\alias{cbk.lame.reduce}
\title{Convert major-element concentration as oxide to one as metal}
\usage{
cbk.lame.reduce(pmlame)
}
\arguments{
\item{pmlame}{A pmlame of element abundances (with columns of stone and rows of chem)}
}
\value{
A pmlame with converted columns
}
\description{
Convert major-element concentration as oxide to one as metal
}
\examples{
pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
message(sprintf("The pmlfile is located at |\%s|.",pmlfile))
pmlame  <- cbk.read.casteml(pmlfile,"ppm",category=NULL,force=TRUE)
pmlame1 <- cbk.lame.reduce(pmlame)

pmlame <- structure(list(Si = c(NA, NA, NA, 0.233, NA, NA, NA, NA), SiO2 = c(0.5195, 0.5359, 0.5341, 0.499, 0.5133, 0.5899, 0.5305, 0.5)), .Names = c("Si", "SiO2"), class = "data.frame", row.names = c("ref-cpx-klb1", "ref-cpx-sax33", "ref-cpx-sax39c", "ref-gl-bhvo2", "ref-gl-dr1a1", "ref-gl-tahiti", "ref-pl-bytownite-c", "ref_dl_blank"))
cbk.lame.reduce(pmlame)
}
