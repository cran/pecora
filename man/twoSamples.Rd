% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twoSamples.R
\name{twoSamples}
\alias{twoSamples}
\title{Permutatation-based two sample t-test}
\usage{
twoSamples(X, B = 1000, alternative, seed = 1234, 
rand = FALSE, permReturn = FALSE, label = NULL)
}
\arguments{
\item{X}{data matrix where rows represent the \code{m} variables and columns the \code{n} observations. The columns' name defines the groups' label.}

\item{B}{numeric value, number of permutations to be performed, including the identity. Default is 1000.}

\item{alternative}{character string referring to the alternative hypothesis (\code{greater}, \code{lower}, \code{two.sided}).}

\item{seed}{numeric value, specify seed. Default is 1234.}

\item{rand}{logical value, \code{TRUE} to compute p-values by permutation distribution.}

\item{permReturn}{logical value, \code{TRUE} to return the t-tests and p-values permutation distribution.}

\item{label}{by default \code{label = NULL}. Labels of the observations, if \code{NULL} the columns's name are considered.}
}
\value{
Returns a list with the following objects:
\describe{ 
  \item{Test}{Vector of \eqn{m} observed one-sample t-tests}
  \item{Test_H0}{Matrix with dimensions \eqn{m \times B} of permuted one-sample t-tests}
  \item{pv}{Vector of \eqn{m} observed p-values} 
  \item{pv_H0}{Matrix with dimensions \eqn{m \times B} of permuted p-values}}
  if \code{permReturn = TRUE} otherwise returns a list with the following objects:
\describe{ 
  \item{Test}{Vector of \eqn{m} observed one-sample t-tests}
  \item{pv}{Vector of \eqn{m} observed p-values}}
}
\description{
Performs two-sample t-tests by permutations.
}
\examples{
X <- matrix(rnorm(100*20), ncol=20)
colnames(X) <- c(rep(0, 10), rep(1,10))
out<- twoSamples(X = X) 
}
\author{
Angela Andreella
}
