
#' @export
shinyscan <- function() {
  shinyApp(ui = ui, server = server, options = "launch.browser")
}
