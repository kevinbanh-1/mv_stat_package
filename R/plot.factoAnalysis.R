#' Factor Analysis Plots
#'
#' @description Function overrides the base plot function for objects of factoAnalysis class. Function creates communality, specific variance, tables, and pairs plots of all the factors.
#'
#'
#' @param x An object of factoAnalysis class
#' @param ... Extra parameters if needed
#'
#' @import ggplot2
#' @import GGally
#'
#' @return Plots of communality, specific variance, and pairs plots. List of loadings from varimax and promax rotations as well as reactable table.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = rnorm(30), y = rnorm(30))
#' obj <- pcfacta(df, m = 1)
#' plot(obj)
#' }
plot.factoAnalysis <- function(x,...){

  if(class(x) != "factoAnalysis"){
    stop("Parameter must be of class factoAnalysis")
  }

  # Get data names
  names <- colnames(x$residual.mat)

  # Create a plot of communalities
  comm.plot <- ggplot(NULL, aes(x = names, y = x$communalities)) +
    geom_point(shape=17, color="orangered", fill="#69b3a2", size=6) +
    ylab("Communality") +
    xlab("Variables") +
    ggtitle("Plot of Communalities") +
    theme(plot.title = element_text(hjust = .5))

  # Create a plot of specific variances
  var.plot <- ggplot(NULL, aes(x = names, y = diag(x$psi))) +
    geom_point(shape=16, color="orangered", fill="#69b3a2", size=6) +
    ylab("Variance") +
    xlab("Variables") +
    ggtitle("Plot of Specific Variance") +
    theme(plot.title = element_text(hjust = .5))

  # Pairs plot of all factor combinations
  pairs.plot <- ggpairs(as.data.frame(unclass(x$loadings)), columnLabels = paste("Factor", c(1:x$m)), title = "Pairs Plot of All Unrotated Factors") +
    theme(plot.title = element_text(hjust = .5)) + xlim(-1,1) + ylim(-1,1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)

  # Calculate loadings after varimax and promax rotation
  var.rot <- varimax(x$loadings)
  pro.rot <- promax(x$loadings)

  # Create matrix of rotated loadings
  rotated <- matrix(c(round(var.rot$loadings,3), round(pro.rot$loadings,3)), ncol = 2*(x$m), byrow = FALSE)
  colnames(rotated) <- c(paste("Varimax Rotated F", sep = "", 1:x$m), paste("Promax Rotated F", sep = "", 1:x$m))

  # Fill in necessary space because of cumalitive proportion row
  empty.spaces <- rep("", x$m)

  rotated <- rbind(rotated,empty.spaces)

  rownames(rotated) <- NULL

  # Combine rotated loadings with original table
  rotated.tab <- cbind(x$table, rotated)


  varimax.plot <- ggpairs(as.data.frame(unclass(var.rot$loadings)), columnLabels = paste("Factor", c(1:x$m)), title = "Varimax Rotated Loadings") +
    theme(plot.title = element_text(hjust = .5))+ xlim(-1,1) + ylim(-1,1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)

  promax.plot <- ggpairs(as.data.frame(unclass(pro.rot$loadings)), columnLabels = paste("Factor", c(1:x$m)), title = "Promax Rotated Loadings") +
    theme(plot.title = element_text(hjust = .5))+ xlim(-1,1) + ylim(-1,1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)


  L <- list(var.rot = var.rot, pro.rot = pro.rot, rotated.table = rotated.tab, var.plot = var.plot, comm.plot = comm.plot, pairs.plot = pairs.plot, vari.plot = varimax.plot, pro.plot = promax.plot)
  invisible(L)

}
