#' @title  pecora-package
#' @name  pecora-package
#' @aliases pecora.package pecorapackage
#' @description The library is devoted to permutation-based inferential methods.
#'
#' The pecora (permutation conditional random) package provides 
#' functions to perform the one-sample and two-samples t-tests using permutations/sign-flipping.
#'
#' The tests comprised are: the one and two samples t-tests.
#'
#' @docType package
#' @author Angela Andreella.
#'
#' Maintainer: Angela Andreella. <angela.andreella@@unipd.it>
#' @references For the general framework of univariate and multivariate
#' permutation tests see: Pesarin, F. (2001) Multivariate Permutation Tests
#' with Applications in Biostatistics. Wiley, New York.
#' 
#' 
#' @keywords package
#' @examples
#'
#' X <- matrix(rnorm(100*20), nrow=20)
#' out <- oneSample(X = X)
#' pv <- t2p(Test = out, alternative = "two.sided")
#' 
#' X <- matrix(rnorm(100*20), nrow=20)
#' rownames(X) <- c(rep(0, 10), rep(1,10))
#' out<- twoSamples(X = X) 
#' pv <- t2p(Test = out, alternative = "two.sided")
#'
NULL
#> NULL

