% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/design.R
\name{check_design}
\alias{check_design}
\title{Check Design}
\usage{
check_design(tomic_design)
}
\arguments{
\item{tomic_design}{a list with named attributes describing feature,
sample, and measurement variables.}
}
\value{
0, invisibly
}
\description{
Check that the design list embedded in `tomic` objects is properly
formatted.
}
\examples{
check_design(brauer_2008_triple$design)

}
