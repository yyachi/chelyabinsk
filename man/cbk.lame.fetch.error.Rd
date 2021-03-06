% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbk.lame.fetch.error.R
\name{cbk.lame.fetch.error}
\alias{cbk.lame.fetch.error}
\title{Extract error columns from pmlame as errorlame}
\usage{
cbk.lame.fetch.error(pmlame, chem = NULL)
}
\arguments{
\item{pmlame}{A pmlame with both mean and error.}

\item{chem}{Target chem you want to extract.}
}
\value{
An errorlame that is a pmlame of only error colums with
  label of `chem' such as `Li7' instead of `Li7_error'.  Note that
  columns that exist in input will be returned.
}
\description{
Extract error columns from pmlame as errorlame
}
\examples{
pmlfile   <- cbk.path("20130528105235-594267.pml")
message(sprintf("The pmlfile is located at |\%s|.",pmlfile))
pmlame    <- cbk.read.casteml(pmlfile)
errorlame <- cbk.lame.fetch.error(pmlame)

errorlame <- cbk.lame.fetch.error(pmlame,c("Li","Lu"))
}
