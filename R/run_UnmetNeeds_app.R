#' Run the UnmetNeeds shiny App
#'
#' @param port_ Positive integer, the TCP port that the application should
#' listen on. See the \code{port} argument in \code{\link[shiny]{runApp}} for
#' more information.
#'
#' @return Returns nothing, but launches a shiny app in a web browser window.
#' @export
#'
#' @examples
#' \dontrun{
#' library(UnmetNeeds)
#' run_UnmetNeeds_app()
#' }
#'
run_UnmetNeeds_app <- function(port_ = NULL) {
  # set application directory
  appDir <- app_sys("app")

  # run the shiny app
  shiny::runApp(
    appDir = appDir,
    port = port_,
    display.mode = "normal"
  )
}
