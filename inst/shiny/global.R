library(fonctionr)
library(shiny)
library(shinyAce)
library(shinyWidgets)
library(shiny.i18n)
library(bslib)
library(bsicons)
library(DT)

obj_design <- getShinyOption("obj_design", NULL)

if (is.null(obj_design)){
  message("Utilisation de eusilc comme données d'exemple")
  message("obj <- svydesign(data=eusilc,ids=~rb030,weights=~rb050)")
  data(eusilc,package="laeken")

  eusilc <- eusilc %>%
    mutate(arpt60 = weighted.mean(eqIncome,rb050)*0.6,
           arop = eqIncome <= arpt60)

  obj_design <- svydesign(data=eusilc,ids=~rb030,weights=~rb050)
}

appDir <- system.file("shiny", package = "fonctionr")
i18n_path  <- getShinyOption("i18n_path",file.path(appDir, "i18n"))

i18n <- shiny.i18n::Translator$new(translation_csvs_path = i18n_path)
i18n$set_translation_language("fr")

source("fonctions.R", local = TRUE)
