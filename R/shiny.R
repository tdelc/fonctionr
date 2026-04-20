#' Shiny fonctionr
#'
#' @returns shinyapp
#' @export
#'
#' @examples
#' \dontrun{
#' data(eusilc,data="laeken")
#' obj <- svydesign(eusilc,ids=~rb030)
#' runShinyFonctionR(obj)
#' }
runShinyFonctionR <- function(obj) {

  appDir <- system.file("shiny", package = "fonctionr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `fonctionr`.", call. = FALSE)
  }

  shiny::shinyOptions(
    obj_design = obj,
    i18n_path  = file.path(appDir, "i18n")
  )

  shiny::runApp(appDir, display.mode = "normal")
  invisible(TRUE)
}
