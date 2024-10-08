% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dim_reduction.R
\name{calculate_sample_mahalanobis_distances}
\alias{calculate_sample_mahalanobis_distances}
\title{Calculate Sample Mahalanobis Distances}
\usage{
calculate_sample_mahalanobis_distances(
  tomic,
  value_var = NULL,
  max_pcs = 10,
  scale = FALSE
)
}
\arguments{
\item{tomic}{Either a \code{tidy_omic} or \code{triple_omic} object}

\item{value_var}{the measurement variable to use for calculating distances}

\item{max_pcs}{the maximum number of principal components to used for
representing the covariance matrix.}

\item{scale}{if TRUE then the data will be scaled before calculating distances}
}
\value{
The samples tibble with a new column `pc_distance` which contains the
  Mahalanobis distances of individual samples from the PC elipsoid
}
\description{
Determine each samples distance from the center of the data using Mahalanobis distance.
}
\details{
Since `romic` is built around using tall data where there are more features than
samples calculating Mahalanobis distance off of the covariance matrix is not
possible. Instead, we use SVD to create a low-dimensional representation of the
covariance matrix and calculate distances from the center of the data in this
space. This essentially involves weighting the principal components by their
loadings.
}
\examples{
calculate_sample_mahalanobis_distances(brauer_2008_tidy)
}
