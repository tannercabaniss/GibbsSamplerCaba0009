#' @title shinyplot
#'
#' @description Calls Gibbs sampler shiny app with descriptive plotting and widgets for
#'
#' @return  Shiny app with contour plot of gibbs sampling
#'
#' @export
#'
#' @examples
#' \dontrun{ shinyplots()}
shinyplots<-function(){
  shiny::runApp(system.file("shiny", package="GibbsSamplerCaba0009"),launch.browser = TRUE)
}
