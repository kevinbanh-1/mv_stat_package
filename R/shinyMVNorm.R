#' @title Normality checking on univariate and multivariate data
#'
#' @description This function calls a shiny app that contains dynamic plots,tables, and outputs relating to normality tests for univariate and muiltivariate
#' data, including proportion test and bivariate test. Plots are created to further aid in checking for normality, with multiple Q-Q plots.
#'
#' @return A shiny web app
#'
#' @import shiny
#'
#' @export
#'
#' @examples
#' \dontrun{shinyMVNorm()}
shinynormality <- function(){
  shiny::runApp(system.file("shinynormal", package = "MATH5793BANH"), launch.browser = TRUE)
}
