#' @title Permutatation-based one sample t-test
#' @description Performs sign-flipped one-sample t-tests.
#' @usage oneSample(X, B = 1000, alternative, seed = 1234, 
#' rand = FALSE, permReturn = FALSE)
#' @param X data matrix where rows represent the \code{m} variables and columns the \code{n} observations.
#' @param B numeric value, number of permutations to be performed, including the identity. Default is 1000.
#' @param alternative character string referring to the alternative hypothesis (\code{greater}, \code{lower}, \code{two.sided}). 
#' @param seed numeric value, specify seed. Default is 1234.
#' @param rand logical value, \code{TRUE} to compute p-values by permutation distribution.
#' @param permReturn logical value, \code{TRUE} to return the t-tests and p-values permutation distribution.
#' @author Angela Andreella
#' @return Returns a list with the following objects:
#' \describe{ 
#'   \item{Test}{Vector of \eqn{m} observed one-sample t-tests}
#'   \item{Test_H0}{Matrix with dimensions \eqn{m \times B} of permuted one-sample t-tests}
#'   \item{pv}{Vector of \eqn{m} observed p-values} 
#'   \item{pv_H0}{Matrix with dimensions \eqn{m \times B} of permuted p-values}}
#'   if \code{permReturn = TRUE} otherwise returns a list with the following objects:
#' \describe{ 
#'   \item{Test}{Vector of \eqn{m} observed one-sample t-tests}
#'   \item{pv}{Vector of \eqn{m} observed p-values}} 
#' @export
#' @importFrom stats pnorm
#' @importFrom matrixStats rowRanks
#' @importFrom Rcpp evalCpp
#' @importFrom stats pt
#' @examples 
#' X <- matrix(rnorm(100*20), ncol=20)
#' out <- oneSample(X = X, alternative = "two.sided")
#' @useDynLib pecora, .registration = TRUE

oneSample <- function(X, B = 1000, 
                      alternative,
                      seed = 1234,rand = FALSE,
                     permReturn = FALSE){

  alternative <- match.arg(tolower(alternative), c("greater", "lower", "two.sided"))
  
  set.seed(seed)

  ## number of obeservation
  n <- ncol(X)
  # number of variables
  m <- nrow(X)
  
  ## Observed test statistics
  rowV <- rowVariance(X)
  Test <- ifelse(rowV==0,0, rowMeans(X)/(sqrt((rowV)/n)))
  
  ## Test statistics under H0
  Test_H0 <- signFlip(X,B-1)
  Test_H0 <- ifelse(is.na(Test_H0), 0 , Test_H0)
  
  if(!rand){
    pv <- switch(alternative, 
                 "two.sided" = 2*(pt(abs(Test), df = n-1, lower.tail=FALSE)),
                 "greater" = pt(Test, df = n-1, lower.tail=FALSE),
                 "lower" = 1-pt(Test, df = n-1, lower.tail=FALSE))
    
    pv_H0 <- switch(alternative, 
                    "two.sided" = 2*(pt(abs(Test_H0), df = n-1,  lower.tail=FALSE)),
                    "greater" = pt(Test_H0, df = n-1,  lower.tail=FALSE),
                    "lower" = 1-pt(Test_H0, df = n-1,  lower.tail=FALSE)) 
  }else{
    
    Test_matrix <- cbind(Test, Test_H0)
    pv_matrix <- switch(alternative, 
                        "two.sided" = rowRanks(-abs(Test_matrix)) / ncol(Test_matrix),
                        "greater" = rowRanks(-Test_matrix) / ncol(Test_matrix),
                        "lower" = rowRanks(Test_matrix) / ncol(Test_matrix))
    
    pv <- pv_matrix[, 1]
    pv_H0 <- pv_matrix[, 2:(B)]
  }
  
  if(permReturn){
    out <- list(Test = Test, Test_H0 = Test_H0, pv = pv, pv_H0 = pv_H0)
  }else{
    out <- list(Test = Test, pv = pv)
  }
  
  
  return(out)
  
}