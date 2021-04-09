#' Univariate Proportion Test of Normality
#'
#' @description This function takes in a data vector/univariate data set and calculates the proportion of data within 1/2 standard deviations of
#' the mean. Then, if the proportion falls within an equality then there are outliers present.
#'
#' @param x Univariate data
#'
#' @return Proportions with 1/2 standard deviations of data set along with result of test.
#'
#' @export
#'
#' @examples
#' x <- c(34,14,151,131,50,76)
#' proptest(x)
#'
proptest <- function(x){
  # x is a data vector
  n <- length(x)
  outliers = FALSE
  idx = 0

  totalp1 = 0
  for(i in 1:n){
    if((x[i] > (mean(x) - sd(x))) & (x[i] < (mean(x) + sd(x)))){
      totalp1 = totalp1 + 1
    }
  }

  p1 <- totalp1/n


  totalp2 = 0
  for(i in 1:n){
    if((x[i] > (mean(x) - 2*sd(x))) & (x[i] < (mean(x) + 2*sd(x)))){
      totalp2 = totalp2 + 1
    }
  }
  p2 <- totalp2/n

  boundary1 <- 1.396/sqrt(n)
  boundary2 <- .628/sqrt(n)

  if((abs(p1- .683) > boundary1)){
    outliers = TRUE
  }

  if((abs(p2 - .954)) > boundary2){
    outliers = TRUE
  }
  return(list(p1 = p1, p2 = p2, outliers.present = outliers))
}
