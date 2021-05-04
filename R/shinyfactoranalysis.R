#' Factor Analysis using Dynamic Plots with Shiny
#'
#' @description This function calls a shiny app which performs factor analysis on a data set. Loadings and variance is calculated and model is tested for adequacy. Plots made to show
#' rotations of loadings.
#'
#' @return A shiny web app
#'
#' @import shiny
#'
#' @export
#'
#' @examples
#' \dontrun{shinyMVNorm()}
shinyfactoranalysis <- function(){
  shiny::runApp(system.file("shinyfactoranalysis", package = "MATH5793BANH"), launch.browser = TRUE)
}
