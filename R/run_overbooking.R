#' Launch the Shiny App Included in This Package
#'
#' This function launches the Shiny application bundled within the MATH4753KORBProject1 package.
#' It assumes the app resides in `inst/app/` and is intended for interactive use.
#'
#' @name run_overbooking
#' @return None. The function launches the app as a side effect.
#' @examples
#' \dontrun{
#'   run_myapp()
#' }
#' @export
run_overbooking <- function() {
  appDir <- system.file("app", package = "MATH4753KORBProject1")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `MATH4753KORBProject1`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
