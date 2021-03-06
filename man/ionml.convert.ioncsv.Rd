% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ionml.convert.ioncsv.R
\name{ionml.convert.ioncsv}
\alias{ionml.convert.ioncsv}
\title{Convert IONCSV to IONML}
\usage{
ionml.convert.ioncsv(ioncsv, outfile = NULL, force = FALSE)
}
\arguments{
\item{ioncsv}{Name of IONCSV.}

\item{outfile}{Name of IONML that will be created.}

\item{force}{Flag to force convert again.}
}
\value{
Name of IONML that was created.
}
\description{
Convert IONCSV to IONML.  This program assumes
  extensions of IONCSV and IONML to be `.ion' and `.xml',
  respectively.

  The IONCSV consists of columns of time and ion intensities.  The
  first column of each line should be number of `cycle'.  Colname
  of the IONCSV should be `time' and name of element followed by
  atomic weight (`Si29' instead of `29Si').
}
\examples{
ioncsv <- cbk.path("ref_cpx_klb1@1.ion")
message(sprintf("The ioncsv is located at |\%s|.",ioncsv))
ionml.convert.ioncsv(ioncsv,outfile=tempfile(fileext=".xml"))
}
\seealso{
\code{ionml.convert.qtegracsv2ioncsv}
}
