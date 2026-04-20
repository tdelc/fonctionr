#' Shiny fonctionr
#'
#' @param obj design object from survey::svydesign
#'
#' @returns shinyapp
#' @export
#'
#' @examples
#' \dontrun{
#' data(eusilc,package="laeken")
#' obj <- svydesign(data=eusilc,ids=~rb030)
#' runShinyFonctionR(obj)
#' }
runShinyFonctionR <- function(obj = NULL) {

  if (is.null(obj)){
    message("Utilisation de eusilc comme données d'exemple")
    message("obj <- svydesign(data=eusilc,ids=~rb030,weights=~rb050)")
    data(eusilc,package="laeken")

    eusilc <- eusilc %>%
      mutate(arpt60 = weighted.mean(eqIncome,rb050)*0.6,
             arop = eqIncome <= arpt60)

    obj <- svydesign(data=eusilc,ids=~rb030,weights=~rb050)
  }

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
