#' Quality Control Ellipse for Bivariate Data
#'
#' @description Function takes in 2 variables in which it calculates the (1-alpha)% quality control ellipse using ggplot's built-in function stat_ellipse.
#'
#' @import ggplot2
#'
#' @param x1 First data variable
#'
#' @param x2 Second data variable
#'
#' @param alpha Rejection level
#'
#' @return A ggplot of a confidence ellipse
#' @export
#'
#' @examples
#' x1 <- rnorm(20)
#' x2 <- rnorm(20)
#' qcellipse(x1,x2)
qcellipse <- function(x1,x2,alpha = .05){
  # alpha = rejection level

  n = length(x1)
  # Plot data and confidence ellipse
  g.ellipse <- qplot(NULL, x = x1, y = x2, color = 1:n) +
    stat_ellipse(geom = "polygon",level = 1-alpha, type = "norm", alpha = 1/10, color = "orangered", aes(fill = "orangered")) +
    theme(legend.position = "none") +
    ylab("x2") +
    xlab("x1") +
    ggtitle(paste0((1-alpha)*100, "% confidence ellipse")) +
    theme(plot.title = element_text(hjust = .5)) +
    scale_color_gradient(low="blue", high="red")



  return(g.ellipse)
}
