#' X-bar Control Chart for Univariate Data
#'
#' @description Function takes in a univariate data set and creates a control chart with horizontal lines showing the lower and upper limits being 3 standard deviations of mean.
#' Points outside of the limts will be out of control.
#'
#' @param x univariate data
#'
#' @import ggplot2
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' data <- c(2,3,4,5,1,3,2)
#' xbarqc(data)
xbarqc <- function(x){

  momts <- c(mean(x), sd(x))

  L = momts[1]-3*momts[2]
  U = momts[1]+ 3*momts[2]

  gq <- ggplot(NULL, aes(x = 1:length(x), y = x, color = 1:length(x))) +
    geom_point() +
    geom_line() +
    ylim(c(momts[1]-4*momts[2],momts[1]+4*momts[2])) +
    ylab("Individual Value") +
    xlab("Observation Number") +
    theme(legend.position = "none") +
    ggtitle("Quality Control Chart") +
    theme(plot.title = element_text(hjust = .5)) +
    scale_color_gradient(low="blue", high="red") +
    geom_hline(yintercept = U, col = "brown1") +
    geom_hline(yintercept = L, col = "brown1") +
    geom_hline(yintercept = momts[1], col = "brown1") +
    geom_text(aes(x=length(x) - 5,y=U,label = paste("UCL = ", round(U,3)))) +
    geom_text(aes(x=length(x) - 5,y=L,label = paste("LCL = ", round(L,3)))) +
    geom_text(aes(x=length(x) - 5,y=momts[1], label = paste("xbar = ", round(momts[1],3))))


  return(list(lower.limit = L, upper.limit = U, moments = momts, qcplot = gq))
}
