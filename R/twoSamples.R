#' @title Permutatation-based two sample t-test
#' @description Performs two-sample t-tests by permutations.
#' @usage twoSamples(X, B = 1000, seed = 1234, 
#' permReturn = TRUE, label = NULL)
#' @param X data matrix where columns represent the \code{m} variables and rows the \code{n} observations. The columns' name defines the groups' label.
#' @param B numeric value, number of permutations to be performed, including the identity. Default is 1000.
#' @param seed numeric value, specify seed. Default is 1234.
#' @param permReturn logical value, \code{TRUE} to return the t-tests and p-values permutation distribution. Default is \code{TRUE}.
#' @param label by default \code{label = NULL}. Labels of the observations, if \code{NULL} the rows's name are considered. D
#' @author Angela Andreella
#' @return Returns a matrix objects:
#' \describe{ 
#'   \item{Test}{Matrix with dimensions \eqn{m x B} of permuted two-samples t-tests. The first column is the observed one-sample t-tests.}}
#'   if \code{permReturn = TRUE} otherwise returns:
#' \describe{ 
#'   \item{Test}{Vector of \eqn{m} observed two-samples t-tests}}
#' @export
#' @importFrom stats pnorm
#' @importFrom matrixStats rowRanks
#' @importFrom stats pt
#' @examples 
#' X <- matrix(rnorm(100*20), nrow=20)
#' rownames(X) <- c(rep(0, 10), rep(1,10))
#' out<- twoSamples(X = X) 


twoSamples <- function(X, B = 1000, 
                      seed = 1234, permReturn = TRUE, 
                      label = NULL){
  
  set.seed(seed)
  
  #If X is a vector we have one variable to test
  if(is.null(dim(X))){X <- matrix(X, ncol = 1)}
  
  #Check labels
  if(is.null(label)){
    if(is.null(rownames(X))){
      stop("Please insert the labels of the observations which describe the two groups to perform the two-samples t-test.")
    }
    label <- rownames(X)}
  
  label <- factor(label)
  levels(label) <- c(0,1)
  ## number of obeservation
  n <- nrow(X)
  # number of variables
  m <- ncol(X)
  
  ## Observed test statistics
  id <- levels(label)
  n1 <- sum(label==id[1])
  n2 <- sum(label==id[2])
  colV1 <- colVariances(X[,label == id[1]])
  colV2 <-colVariances(X[,label == id[2]])
  colM1 <- colMeans(X[,label == id[1]])
  colM2 <-colMeans(X[,label == id[2]])
  pooled.var <- (colV1/n1 + colV2/n2)
  Test <- (colM1 - colM2)/sqrt(pooled.var)
  Test <- ifelse(is.na(Test), 0 , Test)
  
  if(permReturn){
    ## Test statistics under H0
    Test_H0 <- permGroup(as.matrix(t(X)),B-1,label)
    Test_H0 <- ifelse(is.na(Test_H0), 0 , Test_H0)
    Test <- matrix(cbind(Test, Test_H0), ncol = B)
  }
  
  return(Test)
}
