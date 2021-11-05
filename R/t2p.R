#' @title From t-tests to p-values
#' @description Use permutation distribution of a test statistic to get p-values.
#' @usage t2p(Test, alternative = "two.sided", rankBased = TRUE, permReturn = TRUE, df = Inf)
#' @param Test can be a matrix or a vector. 
#' In the first case the columns represent the B permutations and rows the m tests statistic. 
#' The observed test statistic is in the first column
#' and the permutation distribution in the remaining columns.
#' In the second case, it is a vector of length \eqn{m} of observed tests statistics. 
#' If \code{rankBased = TRUE}, you must provide the first option (matrix of permuted statistical tests).
#' @param alternative character string referring to the alternative hypothesis (\code{"greater"}, \code{"lower"}, or \code{"two.sided"}). is \code{"two.sided"}. 
#' @param rankBased logical value, \code{TRUE} to compute p-values by permutation distribution. Default @TRUE.
#' @param permReturn logical value, \code{TRUE} to return the t-tests and p-values permutation distribution. Default @TRUE.
#' @param df numerical value. Degrees of freedom (\eqn{> 0}, maybe non-integer). Default \code{df = Inf}
#' @author Angela Andreella
#' @return Returns an object matrix:
#' \describe{ 
#'   \item{pv}{Matrix with dimensions \eqn{m x B} of permuted one-sample p-values. The first column is the p-values for the observed one-sample t-tests.}}
#' if \code{permReturn = TRUE} otherwise returns:
#' \describe{ 
#'   \item{pv}{Vector of \eqn{m} p-values for the observed one-sample t-tests}}
#' @export
#' @importFrom stats pt
#' @importFrom matrixStats rowRanks
#' @examples 
#' X <- matrix(rnorm(100*20), nrow=20)
#' out <- oneSample(X = X)
#' pv <- t2p(Test = out)

t2p <- function(Test, alternative = "two.sided", 
                rankBased = TRUE, permReturn = TRUE, df = Inf){

  if(!rankBased & (df == Inf)){warning("The degree of freedom equals Inf. The p.values are computed using the Normal distribution instead of the Student's t-distribution.")}
  
  if(rankBased & is.null(dim(Test))){stop("Please insert the matrix of permuted statistical tests")}
  
  if(permReturn){
    
    if(!rankBased){

      pv <- switch(alternative, 
                          "two.sided" = 2*(pt(abs(Test), df = df,  lower.tail=FALSE)),
                          "greater" = pt(Test, df = df,  lower.tail=FALSE),
                          "lower" = 1-pt(Test, df = df,  lower.tail=FALSE)) 
      
    }else{
      
      pv <- switch(alternative, 
                          "two.sided" = rowRanks(-abs(Test)) / ncol(Test),
                          "greater" = rowRanks(-Test) / ncol(Test),
                          "lower" = rowRanks(Test) / ncol(Test))
      
      
    }
    
  }else{
    
    if(!rankBased){
      
      if(!is.null(dim(Test))){Test <- Test[,1]}
      
      pv <- switch(alternative, 
                          "two.sided" = 2*(pt(abs(Test), df = df,  lower.tail=FALSE)),
                          "greater" = pt(Test, df = df,  lower.tail=FALSE),
                          "lower" = 1-pt(Test, df = df,  lower.tail=FALSE)) 
    }else{
      
      pv <- switch(alternative, 
                          "two.sided" = rowRanks(-abs(Test)) / ncol(Test),
                          "greater" = rowRanks(-Test) / ncol(Test),
                          "lower" = rowRanks(Test) / ncol(Test))
      
      pv <- pv[,1]
    }
    
    
  }
  
  return(pv)
}