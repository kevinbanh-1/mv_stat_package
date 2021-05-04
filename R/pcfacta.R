#' Principal Component Factor Analysis
#'
#' @description Function performs factor analysis. User can choose correlation or covariance matrix. Loadings, communalities, residual matrix, and tables are returned. This function also serves
#' as a constructor for analysis pertaining to FA.
#'
#'
#' @param data A data frame
#' @param m The number of factors in the model
#' @param R Set to default TRUE meaning correlation matrix is used. If false, covariance matrix is used
#'
#' @import textutils
#'
#' @return A list of class factoAnalysis containing loadings, communalities, residual matrix, specific variance
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = rnorm(30), y = rnorm(30))
#' MATH5793BANH::pcfacta(df, m = 1)
#' }
pcfacta <- function(data, m, R = TRUE){

  if(!is.data.frame(data)){
    stop("Data must be a data frame")
  }

  if(!is.atomic(m)){
    stop("Number of factors, m, must be a constant")
  }

  # Ensures data contains only quantitative variables
  data <- data[, unlist(lapply(data, is.numeric))]

  if(R){
    mat <- cor(scale(data))
  }
  else{
    mat <- cov(data)
  }


  eig <- eigen(mat)

  p = length(eig$values)

  loadings <- matrix(, nrow = p, ncol = p)

  for(i in 1:p){
    loadings[,i] = sqrt(eig$values[i])*eig$vectors[,i]
  }

  # Select a certain number of factors from loadings
  L <- loadings[,c(1:m)]

  # Construct matrix of specific variances
  psi <- matrix(c(0), nrow = p, ncol = p)
  diag(psi) <- diag(mat - L%*%t(L))

  residual.mat <- mat - (L%*%t(L) + psi)


  communalities <- c()

  # Compute communalities by taking the squared value of each of the loadings in a row and add them together
  for(i in 1:p){
    sum = 0
    for(j in 1:m){
      sum = sum + L[i,j]^2
    }
    communalities[i] = sum
  }



  sum = 0
  cumalitive.prop <- c()
  # Compute proportion of total variance due to m factors
  for(i in 1:m){
    sum = sum + eig$values[i]
    cumalitive.prop[i] <- sum/p
  }

  spec.var <- 1-communalities

  tab <- cbind(round(L,3), round(spec.var,3), round(communalities,3))

  tab <- rbind(tab,c(round(cumalitive.prop,3), spaces(1:(dim(tab)[2] - length(cumalitive.prop)))))


  rownames(tab) <- c(colnames(mat), "Cumalitive proportion of total sample variance")
  colnames(tab) <- c(paste("F", 1:m), "Specific variances", "Communalities")


  factoResult <- list(loadings = L, total.loadings = loadings, psi = psi,residual.mat = residual.mat, communalities = communalities, cumulative.prop = cumalitive.prop, table = tab, m = m, mat = mat, n = dim(data)[1], p = dim(data)[2], data = data)
  class(factoResult) <- "factoAnalysis"
  invisible(factoResult)


}
