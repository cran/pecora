#' @description Internal function.

colVariances <- function (X,na.rm = TRUE) 
{
  sqr = function(X) X * X
  n = colSums(!is.na(X))
  n[n <= 1] = NA
  return(colSums(sqr(X - colMeans(X,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
}