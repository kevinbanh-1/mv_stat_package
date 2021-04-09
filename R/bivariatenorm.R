#' Bivariate Test of Normality
#'
#' @description A more concise version bivariatettest function that only contains output for a t-test. Function takes in 2 data variables, calculates Hotelling's T-square
#' and tests against 50% chi-square quantile. If 50% or more of the data lies within contour, then the pair of variables are normal.
#'
#' @param x1 Univariate data variable
#' @param x2 Univariate data variable
#'
#' @return Result of a bivariate t-test
#'
#' @export
#'
#' @examples
#' x1 <- rnorm(50)
#' x2 <- rnorm(50)
#' bivariatenorm(x1,x2)
bivariatenorm <- function(x1,x2){
  # x1 is a data vector
  # x2 is another data vector



  n <- length(x1)
  dat <- matrix(c(x1,x2), nrow = length(x1), ncol = 2, byrow = FALSE)
  xbar <- colMeans(dat)
  cov <- cov(dat)
  inv <- solve(cov)

  # 50% chi-square contour we want 50% of our data within
  chisq <- qchisq(.5,2)

  # Counters to hold the number of values greater/less than chi-square value
  greater = 0
  less = 0
  norm = FALSE
  for(i in 1:n){
    # Calculate tsq value
    mat <- matrix(c(x1[i] - xbar[1],
                    x2[i] - xbar[2]), ncol = 1)
    tsq <- t(mat)%*%inv%*%mat

    # Check if tsq value is within the 50% chi-square contour
    if(tsq <= chisq){
      less = less + 1
    }

    if(tsq > chisq){
      greater = greater + 1
    }
  }
  if(less/n >= .5){
    norm = TRUE
  }

  return(list(number.greater = greater,
              number.less = less, normal = norm))
}
