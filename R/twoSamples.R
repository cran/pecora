#' @title Permutatation-based two sample t-test
#' @description Performs two-sample t-tests by permutations.
#' @usage twoSamples(X, B = 1000, alternative, seed = 1234, 
#' rand = FALSE, permReturn = FALSE, label = NULL)
#' @param X data matrix where rows represent the \code{m} variables and columns the \code{n} observations. The columns' name defines the groups' label.
#' @param B numeric value, number of permutations to be performed, including the identity. Default is 1000.
#' @param alternative character string referring to the alternative hypothesis (\code{greater}, \code{lower}, \code{two.sided}). 
#' @param seed numeric value, specify seed. Default is 1234.
#' @param rand logical value, \code{TRUE} to compute p-values by permutation distribution.
#' @param permReturn logical value, \code{TRUE} to return the t-tests and p-values permutation distribution.
#' @param label by default \code{label = NULL}. Labels of the observations, if \code{NULL} the columns's name are considered.
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
#' @importFrom stats pt
#' @examples 
#' X <- matrix(rnorm(100*20), ncol=20)
#' colnames(X) <- c(rep(0, 10), rep(1,10))
#' out<- twoSamples(X = X) 


twoSamples <- function(X, B = 1000, alternative = "two.sided", 
                      seed = 1234,rand = FALSE, permReturn = FALSE, 
                      label = NULL){
  
  if(!is.null(seed)){set.seed(seed)}
 # alternative_set <- c("greater", "lower", "two.sided")
 # alternative <- match.arg(tolower(alternative), alternative_set)
  
  if(is.null(label)){label <- colnames(X)}
  
  label <- factor(label)
  levels(label) <- c(0,1)
  ## number of obeservation
  n <- ncol(X)
  # number of variables
  m <- nrow(X)
  
  ## Observed test statistics
  id <- levels(label)
  n1 <- sum(label==id[1])
  n2 <- sum(label==id[2])
  rowV1 <- rowVariance(X[,label == id[1]])
  rowV2 <-rowVariance(X[,label == id[2]])
  rowM1 <- rowMeans(X[,label == id[1]])
  rowM2 <-rowMeans(X[,label == id[2]])
  pooled.var <- (rowV1/n1 + rowV2/n2)
  Test <- (rowM1 - rowM2)/sqrt(pooled.var * (1/n1 + 1/n2))
  Test <- ifelse(is.na(Test), 0 , Test)
  ## Test statistics under H0
  Test_H0 <- permGroup(as.matrix(X),B-1,label)
  Test_H0 <- ifelse(is.na(Test_H0), 0 , Test_H0)
  
  if(!rand){
    
    gdl <- ((rowV1/n1 + rowV2/n2)^2)/((((rowV1/n1)^2)/(n1-1))+(((rowV2/n2)^2)/(n2-1)))
    pv <- switch(alternative, 
                 "two.sided" = 2*(pt(abs(Test), df = gdl, lower.tail=FALSE)),
                 "greater" = pt(Test, df = gdl, lower.tail=FALSE),
                 "lower" = 1-pt(Test, df = gdl, lower.tail=FALSE))
    
    pv_H0 <- switch(alternative, 
                    "two.sided" = 2*(pt(abs(Test_H0), df = gdl, lower.tail=FALSE)),
                    "greater" = pt(Test_H0, df = gdl, lower.tail=FALSE),
                    "lower" = 1-pt(Test_H0, df = gdl, lower.tail=FALSE))
  }else{
    
    Test_matrix <- cbind(Test, Test_H0)
    pv_matrix <- switch(alternative, 
                        "two.sided" = rowRanks(-abs(Test_matrix)) / (B),
                        "greater" = rowRanks(-Test_matrix) / (B),
                        "lower" = rowRanks(Test_matrix) / (B))
    
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
