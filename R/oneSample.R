#' @title Permutatation-based one sample t-test
#' @description Performs sign-flipped one-sample t-tests.
#' @usage oneSample(X, B = 1000, seed = 1234, permReturn = TRUE)
#' @param X data matrix where columns represent the \code{m} variables and rows the \code{n} observations.
#' @param B numeric value, number of permutations to be performed, including the identity. Default is 1000.
#' @param seed numeric value, specify seed. Default is 1234.
#' @param permReturn logical value, \code{TRUE} to return the t-tests permutation distribution. Default is \code{TRUE}.
#' @author Angela Andreella
#' @return Returns an object matrix:
#' \describe{ 
#'   \item{tv}{Matrix with dimensions \eqn{m x B} of permuted one-sample t-tests. The first column is the observed one-sample t-tests.}}
#'   if \code{permReturn = TRUE} otherwise returns:
#' \describe{ 
#'   \item{tv}{Vector of \eqn{m} observed one-sample t-tests}}
#' @export
#' @importFrom Rcpp evalCpp
#' @importFrom stats pt
#' @examples 
#' X <- matrix(rnorm(100*20), nrow=20)
#' out <- oneSample(X = X)
#' @useDynLib pecora, .registration = TRUE

oneSample <- function(X, B = 1000, 
                      seed = 1234,
                      permReturn = TRUE){

  set.seed(seed)

  #If X is a vector we have one variable to test
  if(is.null(dim(X))){X <- matrix(X, ncol = 1)}
  ## number of obeservation
  n <- nrow(X)
  # number of variables
  m <- ncol(X)
  
  ## Observed test statistics
  colV <- colVariances(X)
  Test <- ifelse(colV==0,0, colMeans(X)/(sqrt((colV)/n)))
  if(permReturn){
    ## Test statistics under H0
    Test_H0 <- signFlip(t(X),B-1)
    Test_H0 <- ifelse(is.na(Test_H0), 0 , Test_H0)
    Test <- matrix(cbind(Test, Test_H0), ncol = B)
  }

  return(Test)
  
}