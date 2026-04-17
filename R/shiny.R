#' Shiny fonctionr
#'
#' @returns shinyapp
#' @export
#'
#' @examples
#' \dontrun{runVizsurvey()}
runShinyFonctionR <- function(obj) {

  appDir <- system.file("shiny", package = "fonctionr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `fonctionr`.", call. = FALSE)
  }

  shiny::shinyOptions(obj_design = obj)
  shiny::runApp(appDir, display.mode = "normal")

  invisible(TRUE)
}
