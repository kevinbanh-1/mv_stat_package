#' Bootstrapping of Principal Components
#'
#' @description This function performs bootstrapping on sample principal components of a given data set with the package "boot" and creates density plots for the eigenvalues and eigenvectors.
#'
#' @param data A data set with quantitative variables
#' @param alpha Significance level
#' @param iter Number of bootstrap iterations
#' @param cov If TRUE uses S. Else, use R (correlation matrix).
#'
#' @import boot
#' @import graphics
#' @import grDevices
#'
#' @return Histogram plots, boxplot, and output for bootstrapping of PCA and confidence intervals
#' @export
#'
#' @examples
#' df <- data.frame(x = rnorm(50), y = rnorm(50))
#' pcabootAG(df,.05,10000)
pcabootAG <- function(data,alpha,iter,cov = TRUE){

  n = dim(data)[1]
  p = dim(data)[2]

  # Following two functions calculate eigenvalue/vector statistic
  eigenvec <- function(data,indices){
    S <- cov(data[indices,])
    R <- cor(cov(data[indices,]))
    if(cov == TRUE){
      E <- eigen(S)
    }
    else{
      E <- eigen(R)
    }
    e <- E$vectors
    return(c(e))
  }
  eigenval <- function(data, indices){
    S <- cov(data[indices,])
    R <- cor(cov(data[indices,]))
    if(cov == TRUE){
      E <- eigen(S)
    }
    else{
      E <- eigen(R)
    }

    lambda <-E$values
    return(c(lambda))
  }

  # Bootstrap
  eigenval.boot <- boot(data,eigenval,R = iter)
  eigenvec.boot <- boot(data,eigenvec,R = iter)

  # Layout for histogram plots
  lo <- layout(matrix(c(1:p), nrow = 1))

  # List to hold bootstrap confidence intervals for lambda
  bootstrap.ci <- list()

  # List to hold bootstrap confidence intervals for eigen vectors
  bootstrapEV.ci <- list()

  # Function to calculate A and G confidence intervals for lambda
  eigenval.ci <- function(lambda,alpha){
    ci <- c()
    ci[1]<- lambda/(1+qnorm(1-alpha/2)*sqrt(2/n))
    ci[2]<- lambda/(1-qnorm(1-alpha/2)*sqrt(2/n))
    return(ci)
  }

  # List to hold lambda confidence intervals
  andgir <- list()



  for(i in 1:p){
    h <- hist(eigenval.boot$t[,i], plot = FALSE)
    d <- h$density
    den <- d/max(d)
    hist(eigenval.boot$t[,i], freq = FALSE,
         col = rgb(0,den,1-den^2, 0.7),
         main = paste("Bootstrap distribution of lambda", i),
         xlab = paste("lambda", i))

    # Calculate bootstrap ci
    bootstrap.ci[[i]] <- quantile(eigenval.boot$t[,i],c(alpha/2, 1-alpha/2))
    bootstrapEV.ci[[i]] <- quantile(eigenvec.boot$t[,i],c(alpha/2, 1-alpha/2))

    S <- cov(data)
    R <- cor(cov(data))
    if(cov == TRUE){
      E <- eigen(S)
    }
    else{
      E <- eigen(R)
    }

    # Calculate confidence intervals for lambda
    andgir[[i]] <- eigenval.ci(E$values[i], alpha)
  }


  cat <- rep(c(1,2), iter*(c(1,1)))
  cat <- matrix(cat,nrow = iter, ncol = 2, byrow = FALSE)

  lo <- layout(matrix(c(1)))
  boxplot(eigenvec.boot$t ~ cat,
          xlab = "Eigenvectors",
          ylab = "Value")



  return(list(eigenval.boot = eigenval.boot, eigenvec.boot = eigenvec.boot, eigvalboot.ci = bootstrapEV.ci,bootstraplambda.ci = bootstrap.ci, andgir.ci = andgir))
}
