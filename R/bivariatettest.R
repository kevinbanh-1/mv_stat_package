#' Bivariate T-test and Corresponding Confidence Intervals
#'
#' @param data A data set with at least 2 qualitative variables
#' @param mu0 The proposed null hypothesis mean values
#' @param alpha The rejection region.
#'
#' @return The result of a bivariate t-test. If the hypothesis is in the ellipse, then accept the null. Additionally,
#' eigenvalues and vectors, plot of confidence ellipse, major minor axis and ratio.
#'
#' @import stats
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{bivariate.t(rad.df, mu0 = c(.562,.589), alpha = .05)}
bivariatettest <- function(data, mu0, alpha = .05){
  # data = data set
  # mu0 = vector of means
  # alpha = rejection level

  # Ensure matrix format of data and obtain number of rows and columns of data
  dat <- as.matrix(data)
  p <- ncol(dat)
  n <- nrow(dat)

  # Find covariance, inverse of covariance matrix, and eigenvectors/values
  cov <- cov(dat)
  inv <- solve(cov)
  eigen <- eigen(cov)
  eigenval <- eigen(cov)$values

  # Hotelling's T square
  tsq <- n%*%t(colMeans(dat) - mu0)%*%inv%*%(colMeans(dat) - mu0)
  f.calc <- (p*(n-1))/(n-p)*qf(1- alpha, p, n-p)

  # Perform T-test. Check whether statistic is less than f statistic
  inEllipse = FALSE
  if(tsq <= f.calc){
    inEllipse = TRUE
  }

  # Length of major and minor axes along with ratio
  major <- sqrt(eigenval[1])*sqrt((p*(n-1))/(n-p)*f.calc)
  minor <- sqrt(eigenval[2])*sqrt((p*(n-1))/(n-p)*f.calc)
  ratio <- sqrt(eigenval[1])/sqrt(eigenval[2])

  # Plot data and confidence ellipse
  g.ellipse <- qplot(data = data, x = data[,1], y = data[,2], color = 1:n) +
    stat_ellipse(level = 1-alpha) +
    ylab(names(data)[2]) +
    xlab(names(data)[1]) +
    ggtitle(paste0(1-alpha, " confidence ellipse")) +
    theme(plot.title = element_text(hjust = .5))



  return(list(t.test.accept = inEllipse, eigen = eigen, major.half.axis = major, minor.half.axis = minor, major.minor.ratio = ratio,
              confidence.ellipse = g.ellipse))




}
