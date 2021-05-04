#' Bonferroni confidence interval for eigen values
#'
#' @description Function generates the bonferroni Anderson and Girshik confidence intervals for the eigen values of a data set
#'
#' @param data A data set containing quantitative variables
#' @param q Number of eigen values we want to generate confidence intervals for
#' @param alpha Significance level
#'
#' @return List containing bonferroni confidence intervals
#' @export
#'
#' @examples
#' df <- data.frame(x = rnorm(30), y = rnorm(30), z = rnorm(30))
#' bonf.andgir.cis(df,3,.10)
bonf.andgir.cis <- function(data, q, alpha){

  n = dim(data)[1]
  p = dim(data)[2]

  eigen.val <- c()
  for(i in 1:q){
    eigen.val[i] = eigen(cov(data))$value[i]
  }

  bonf.ci <- list()
  for(i in 1:q){
    zcalc <- qnorm(1-alpha/(2*q))
    lower.lim <- eigen.val[i]/(1 + zcalc*sqrt(2/n))
    upper.lim <- eigen.val[i]/(1 - zcalc*sqrt(2/n))
    bonf.ci[[i]] = c(lower.lim, upper.lim)
  }

  return(bonf.ci)
}
