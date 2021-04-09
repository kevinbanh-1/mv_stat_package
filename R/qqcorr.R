#' Correlation Coefficient for a Q-Q plot
#'
#' @param x Univariate data vector
#'
#' @return A correlation coefficient value -- cannot exceed 1
#' @export
#'
#' @examples
#' x <- rnorm(20)
#' qqcorr(x)
qqcorr <- function(x){
  # x is a data vector

  # Order observations
  x <- sort(x)

  sum_num = 0
  sum_denom1 = 0
  sum_denom2 = 0
  n = length(x)
  for(i in 1:n){
    # Create quantiles for each ith iteration
    quant <- qnorm((i-1/2)/n)

    # Mean corrected
    sum_num = sum_num + ((x[i] - mean(x))*(quant))
    sum_denom1 = sum_denom1 + ((x[i] - mean(x))^2)
    sum_denom2 = sum_denom2 + quant^2
  }

  rq = sum_num/(sqrt(sum_denom1)*sqrt(sum_denom2))
  return(rq)
}
