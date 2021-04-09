#' T-Square Quality Control Chart for Multivariate Data
#'
#' @description This function takes in multivariate data and calculates Hotelling's T-square then plots it against the number of observations to obtain a quality control
#' chart. The limits are obtained from chi-square quantiles for 95% and 99% confidence level.
#'
#' @import ggplot2
#' @import latex2exp
#'
#' @param x Multivariate data frame with 2+ variables
#'
#' @return A quality control ggplot chart
#' @export
#'
#' @examples
#' df <- data.frame(x = rnorm(20), y = rnorm(20))
#' t2chart(df)
t2chart <- function(x){
  # x is a data frame/matrix

  # Get rows and dimensions of data
  n = dim(x)[1]
  p = dim(x)[2]
  cov <- cov(x)
  inv <- solve(cov)
  xbar <- colMeans(x)

  # Vector to hold Hotelling's T-square values
  tsq.val <- c()

  # Find T-square value for each pair of data entries
  for(i in 1:n){
    # Subtract mean from pair of variables
    centered <- as.matrix(x[i,] - xbar)

    # Calculate tsq values
    tsq = centered%*%inv%*%t(centered)
    tsq.val[i] = tsq
  }

  # Find 95% and 99% confidence limits
  lim95 <- qchisq(1-.05,df = p)
  lim99 <- qchisq(1-.01,df = p)

  # Plot tsq values against number of samples
  g <- ggplot(NULL, aes(x = 1:n, y = tsq.val, color = 1:n)) +
    geom_point() +
    xlab("Case") +
    ylab(TeX("$T^2$")) +
    theme(plot.title = element_text(hjust = .5)) +
    scale_color_gradient(low="blue", high="red") +
    theme(legend.position = "none") +
    geom_hline(yintercept = lim95, col = "brown1", linetype = "dotted") +
    geom_hline(yintercept = lim99, col = "brown1") +
    geom_text(aes(x = n, y = lim95, label = "95% limit")) +
    geom_text(aes(x = n, y = lim99, label = "99% limit"))


  return(g)
}
