% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drug_timer.default.R
\name{drug_timer.default}
\alias{drug_timer.default}
\title{Default method for drug_timer()}
\usage{
\method{drug_timer}{default}(adm, change, drug)
}
\arguments{
\item{adm}{admission data set cleaned using `tbcleanr`}

\item{change}{change data set cleaned using `tbcleanr`}

\item{drug}{define which drug to calculate}
}
\description{
Allow data frames with unspecified object class to pass through
}
\seealso{
\code{\link{tbgeneratr}}
}
\author{
Jay Achar
}
