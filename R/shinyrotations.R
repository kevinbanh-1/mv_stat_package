#' @title Dynamic plots using Shiny
#'
#' @description The app contains various widgets based on the input .csv file given. The user can then choose the variables to plot
#' and be able to calculate the correlations based on which point they want to drop. Additionally, the user will be able to see which root for
#' which the covariance is 0 given axes rotations.
#'
#' @return Shiny app
#' @export
#'
#' @examples
#' \dontrun{shinyapp()}
shinyrotations <- function(){
  shiny::runApp(system.file("shinyapp", package = "MATH5793BANH"), launch.browser = TRUE)
}
