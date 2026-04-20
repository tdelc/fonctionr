library(shiny)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(shinyAce)
library(tidyverse)
library(fonctionr)
library(gt)
library(DT)
library(shiny.i18n)
library(survey)
library(srvyr)

obj_design <- getShinyOption("obj_design", NULL)
i18n_path  <- getShinyOption("i18n_path",
                             system.file("shiny/i18n", package = "fonctionr"))

i18n <- shiny.i18n::Translator$new(translation_csvs_path = i18n_path)
i18n$set_translation_language("fr")

source("fonctions.R", local = TRUE)
