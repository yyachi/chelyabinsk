% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbk.lame.normalize.R
\name{cbk.lame.normalize}
\alias{cbk.lame.normalize}
\title{Return normalized element abundances}
\usage{
cbk.lame.normalize(pmlame, reflame, suffix_after_chem = NULL,
  verbose = FALSE)
}
\arguments{
\item{pmlame}{A pmlame with row of stone and column of chem [g/g].}

\item{reflame}{A pmlame of a reference.  This can be a pmlame of
multiple references.}

\item{suffix_after_chem}{String to recognize column of errors.
Feed "_error" when necessary.  As of February 18, 2017, this
exists only for compatibility.}

\item{verbose}{Output debug info (default: FALSE).}
}
\value{
A ref-normalized daraframe with only elements defined in
  ref.
}
\description{
Return normalized element abundances.  Note that only
  elements that exist both in pmlame and reflame are processed.
  See also "Geochemical Modelling..." by Janousek et al. (2015)
}
\examples{
dflame.csv <- cbk.path("20081202172326.hkitagawa_trace.dflame")
message(sprintf("The dflame.csv is located at |\%s|.",dflame.csv))
pmlame     <- cbk.read.dflame(dflame.csv,"ppm")
reflame    <- cbk.ref("Boynton.1989","ppm")
cbk.lame.normalize(pmlame,reflame)
}
\seealso{
\link{cbk.ref} and \link{cbk.periodic}
}
