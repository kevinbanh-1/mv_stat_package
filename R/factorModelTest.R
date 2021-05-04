#' Large Sample Test of Adequacy for an M-Factor Model
#'
#' @description Function calculates the test statistic with Bartlett's correction against the chi-square quantile. If the test statistic is greater than the chi-square
#' quantile, then we reject the null that the M-factor model is appropriate for the data in favor of some other positive definite matrix. Results are then formatted into a gt table.
#'
#' @param x An object of class factoAnalysis
#' @param alpha Significance level
#' @param ... Extra parameters if needed
#'
#' @import gt
#' @import kableExtra
#' @importFrom magrittr %>%
#'
#' @return A matrix with results of test
#' @export
#'
#' @examples
#' \dontrun{
#' df = data.frame(x = rnorm(20), y = rnorm(20), z = rnorm(20))
#' obj <- pcfacta(df, m = 2),
#' factorModelTest(obj, .05)
#' }
factorModelTest <- function(x, alpha,...){


  if(class(x) != "factoAnalysis"){
    stop("Parameter must be of class factoAnalysis")
  }

  # Obtain maximum likelihood loadings
  mle <- factanal(x$data, factors = x$m, rotation = "none", method = "mle")
  mle.loadings <- mle$loadings
  mle.psi <- mle$uniquenesses

  if(x$m < (1/2)*(2*x$p + 1 - sqrt(8*x$p + 1))){ # Ensure degrees of freedom is positive

    rowcol = length(mle.psi)
    psi <- matrix(c(0), nrow = rowcol, ncol = rowcol)
    diag(psi) <- mle.psi

    model <- mle.loadings%*%t(mle.loadings) + psi


    # Calculate test statstic
    est <- (x$n-1 - (2*x$p + 4*x$m + 5)/6)*log(det(model)/det(x$mat))

    # Calculate chi-square statistic
    chi.stat <- qchisq(1-alpha, ((x$p - x$m)^2 - x$p - x$m)/2)

    # Whether to reject or accept H0 that m-factor model is adequate
    if(est < chi.stat){
      result = "FALSE"
    }
    else{
      result = "TRUE"
    }
  }



  result.mat <- matrix(c(round(est,3), round(chi.stat,3), result), nrow = 3, ncol = 1)
  rownames(result.mat) <- c("Test Statistic with Bartlett's Correction", "Chi-square Statistic", paste("Reject F", sep = "", x$m, " model"))
  colnames(result.mat) <- "Result"

  return(result.mat)
}
