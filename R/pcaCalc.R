#' Principal Component Analysis Calculation
#'
#' @description This function performs principal component analysis. It constructs a sample covariance matrix, finds sample principal components, total sample variance,
#' and a cor(yi,xk) matrix.
#'
#' @param data A data set containing quantitative variables
#'
#' @return A list containing sample covariance matrix, sample principal components, total sample variance, and a correlation matrix
#'
#' @export
#'
#' @examples
#' df <- data.frame(x = rnorm(30), y = rnorm(30), z = rnorm(30))
#' pcaCalc(df)
pcaCalc <- function(data){
  cov <- cov(data)
  eig <- eigen(cov)

  n <- dim(cov)[1]
  p <- dim(cov)[2]

  rho <- matrix(, nrow = n, ncol = p)

  for(i in 1:n){
    for(j in 1:p){
      rho[i,j] = eig$vectors[i,j]*sqrt(eig$values[i])/sqrt(cov[j,j])
    }
  }

  total.sample.var <- sum(eig$values)

  return(list(S = cov, sample.princ.comp = eig$vectors, variance = eig$values, total.variance = total.sample.var, correlation = rho))
}
