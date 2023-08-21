#' Check shinyapps.io Account Secrets
#'
#' @description
#' a function to stop the script when one of the variables cannot be found.
#' and to strip quotation marks from the secrets when you supplied them.
#'
#' @param name_ The name of the secret/variable to find
#'
#' @return Unquoted value of the secret
#' @export
#'
#' @examples
#' \dontrun{
#' error_on_missing_name(name_ = "app")
#' }
error_on_missing_name <- function(name_) {
  var <- Sys.getenv(name_, unset = NA)

  if(is.na(var)) {
    stop(paste0("cannot find ", name_, " !"), call. = FALSE)
  }
  gsub("\"", '', var)

}

#' Access files in the current app
#'
#' @param ... character vectors, specifying sub-directory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "UnmetNeeds")
}
