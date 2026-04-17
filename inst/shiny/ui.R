library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyAce)
library(tidyverse)
library(fonctionr)
library(gt)
library(DT)
library(shiny.i18n)
library(tidyverse)
library(survey)
library(srvyr)

source("fonctions.R")

# -----------------------------------------------------------------------------
# 1. UI : tableau de bord
# -----------------------------------------------------------------------------

ui <- tagList(
  usei18n(i18n),
  dashboardPage(
    header = dashboardHeader(title = "FonctionR"),
    sidebar = dashboardSidebar(
      selectInput(
        inputId='lang',
        label=i18n$t('Changer la langue'),
        choices = i18n$get_languages(),
        selected = i18n$get_key_translation()
      ),
      sidebarMenu(
        menuItem(i18n$t("Proportions"), tabName = "prop", icon = icon("pie-chart")),
        menuItem(i18n$t("Distributions"), tabName = "distrib", icon = icon("bars")),
        menuItem(i18n$t("Moyennes"), tabName = "mean", icon = icon("chart-line")),
        menuItem(i18n$t("Comparaisons multiples"), tabName = "many", icon = icon("layer-group")),
        menuItem(i18n$t("Distributions continues"), tabName = "cont", icon = icon("wave-square")),
        menuItem(i18n$t("Graphique libre"), tabName = "free", icon = icon("lightbulb")),
        menuItem(i18n$t("Distributions des revenus"), tabName = "distribution", icon = icon("chart-simple"))
      )
    ),
    body = dashboardBody(
      tabItems(

        # ───────────────────────────────────────────────
        # Onglet 1 : Proportions
        # ───────────────────────────────────────────────
        tabItem(tabName = "prop",
                fluidRow(
                  box(width = 3, title = i18n$t("Paramètres"), solidHeader = TRUE, status = "primary",
                      selectInput("prop_var", i18n$t("Variable à expliquer (binaires) :"), choices = NULL),
                      selectInput("prop_group", i18n$t("Variable de groupe :"), choices = NULL),
                      selectInput("prop_fill", i18n$t("Sous-groupe (optionnel) :"), choices = c("Aucun")),
                      textInput("prop_title", i18n$t("Titre du graphique :"), "Proportion par groupe"),
                      actionButton("prop_run", i18n$t("Exécuter"), icon = icon("play"), class = "btn-success")
                  ),
                  box(width = 9, title = i18n$t("Résultats"), status = "primary", solidHeader = TRUE,
                      tabsetPanel(
                        tabPanel(i18n$t("Graphique"), plotOutput("prop_graph", height = "600px")),
                        tabPanel(i18n$t("Tableau"), DTOutput("prop_tab")),
                        tabPanel(i18n$t("Tableau de synthèse"), gt_output("prop_gt")),
                        tabPanel(i18n$t("Code R"), verbatimTextOutput("prop_code"))
                      ),
                      gt_output("prop_low")
                  )
                )
        ),

        # ───────────────────────────────────────────────
        # Onglet 2 : Distributions
        # ───────────────────────────────────────────────
        tabItem(tabName = "distrib",
                fluidRow(
                  box(width = 3, title = i18n$t("Paramètres"), solidHeader = TRUE, status = "primary",
                      selectInput("distrib_var", i18n$t("Variable catégorielle :"), choices = NULL),
                      selectInput("distrib_group", i18n$t("Groupe (optionnel) :"), choices = c("Aucun")),
                      textInput("distrib_title", i18n$t("Titre :"), "Distribution par groupe"),
                      actionButton("distrib_run", i18n$t("Exécuter"), icon = icon("play"), class = "btn-success")
                  ),
                  box(width = 9, title = i18n$t("Résultats"), status = "primary", solidHeader = TRUE,
                      tabsetPanel(
                        tabPanel(i18n$t("Graphique"), plotOutput("distrib_graph", height = "600px")),
                        tabPanel(i18n$t(i18n$t("Tableau")), DTOutput("distrib_tab")),
                        tabPanel(i18n$t("Tableau de synthèse"), gt_output("distrib_gt")),
                        tabPanel(i18n$t("Code R"), verbatimTextOutput("distrib_code"))
                      )
                  )
                )
        ),

        # ───────────────────────────────────────────────
        # Onglet 3 : Moyennes
        # ───────────────────────────────────────────────
        tabItem(tabName = "mean",
                fluidRow(
                  box(width = 3, title = i18n$t("Paramètres"), solidHeader = TRUE, status = "primary",
                      selectInput("mean_var", i18n$t("Variable quantitative :"), choices = NULL),
                      selectInput("mean_group", i18n$t("Groupe :"), choices = c(NULL)),
                      textInput("mean_title", i18n$t("Titre :"), "Moyenne par groupe"),
                      actionButton("mean_run", i18n$t("Exécuter"), icon = icon("play"), class = "btn-success")
                  ),
                  box(width = 9, title = i18n$t("Résultats"), status = "primary", solidHeader = TRUE,
                      tabsetPanel(
                        tabPanel(i18n$t("Graphique"), plotOutput("mean_graph", height = "600px")),
                        tabPanel(i18n$t("Tableau"), DTOutput("mean_tab")),
                        tabPanel(i18n$t("Tableau de synthèse"), gt_output("mean_gt")),
                        tabPanel(i18n$t("Code R"), verbatimTextOutput("mean_code"))
                      )
                  )
                )
        ),

        # ───────────────────────────────────────────────
        # Onglet 4 : Comparaisons multiples
        # ───────────────────────────────────────────────
        tabItem(tabName = "many",
                fluidRow(
                  box(width = 3, title = i18n$t("Paramètres"), solidHeader = TRUE, status = "primary",
                      uiOutput("many_vars_ui"),
                      selectInput("many_type", i18n$t("Type de calcul :"),
                                  choices = c("Mean"="many_mean",
                                              "Median"="many_median",
                                              "Proportion"="many_prop")),
                      textInput("many_title", i18n$t("Titre :"), "Comparaison de plusieurs indicateurs"),
                      actionButton("many_run", i18n$t("Exécuter"), icon = icon("play"), class = "btn-success")
                  ),
                  box(width = 9, title = i18n$t("Résultats"), status = "primary", solidHeader = TRUE,
                      tabsetPanel(
                        tabPanel(i18n$t("Graphique"), plotOutput("many_graph", height = "600px")),
                        tabPanel(i18n$t("Tableau"), DTOutput("many_tab")),
                        tabPanel(i18n$t("Tableau de synthèse"), gt_output("many_gt")),
                        tabPanel(i18n$t("Code R"), verbatimTextOutput("many_code"))
                      )
                  )
                )
        ),

        # ───────────────────────────────────────────────
        # Onglet 5 : Distribution continue
        # ───────────────────────────────────────────────
        tabItem(tabName = "cont",
                fluidRow(
                  box(width = 3, title = i18n$t("Paramètres"), solidHeader = TRUE, status = "primary",
                      selectInput("cont_var", i18n$t("Variable continue :"), choices = NULL),
                      selectInput("cont_group", i18n$t("Groupe (optionnel) :"), choices = c("Aucun")),
                      selectInput("cont_type", i18n$t("Type :"), choices = c("mean","median")),
                      numericInput("cont_bw", i18n$t("Largeur de bande (bw)"), value = 0.7, min = 0.1, max = 2, step = 0.1),
                      numericInput("cont_min", i18n$t("Minimum"), value = 0),
                      numericInput("cont_max", i18n$t("Maximum"), value = 100000),
                      textInput("cont_title", i18n$t("Titre :"), "Distribution d'une variable numérique"),
                      actionButton("cont_run", i18n$t("Exécuter"), icon = icon("play"), class = "btn-success")
                  ),
                  box(width = 9, title = i18n$t("Résultats"), status = "primary", solidHeader = TRUE,
                      tabsetPanel(
                        tabPanel(i18n$t("Graphique"), plotOutput("cont_graph", height = "600px")),
                        tabPanel(i18n$t("Tableau des quantiles"), DTOutput("cont_quant")),
                        tabPanel(i18n$t("Données de densité"), DTOutput("cont_dens")),
                        tabPanel(i18n$t("Code R"), verbatimTextOutput("cont_code"))
                      )
                  )
                )
        ),

        # ───────────────────────────────────────────────
        # Onglet 6 : Graphique libre
        # ───────────────────────────────────────────────
        tabItem(tabName = "free",
                fluidRow(
                  box(width = 3, title = i18n$t("Code"), solidHeader = TRUE,
                      status = "primary",
                      radioGroupButtons(
                        inputId = "free_template",
                        label = "Choisir un template",
                        choices = c("Distribution catégorielle",
                                    "Distribution catégorielle par groupe",
                                    "Distribution continue",
                                    "Distribution continue par groupe",
                                    "Proportion par groupe","Stat par groupe",
                                    "Plusieurs indicateurs",
                                    "Plusieurs indicateurs par groupe"),
                        justified = FALSE,size = "xs"
                      ),
                      textAreaInput(
                        inputId = "free_questionAI",
                        label = "Aide à l'écriture de code par IA",
                        height = "100px",
                        placeholder = "Poser votre question à une IA"
                      ),
                      actionButton("free_askAI", i18n$t("Générer le code"), icon = icon("play"), class = "btn-warning"),br(),br(),
                      aceEditor(
                        outputId = "free_code",
                        value = "",
                        mode = "r",
                        theme = "chrome",   # ou ambiance, etc
                        height = "200px",
                        showLineNumbers = FALSE,
                        wordWrap = TRUE,
                        fontSize = 14
                      ),
                      actionButton("free_run", i18n$t("Exécuter le code"), icon = icon("play"), class = "btn-success"),
                      textOutput("free_vars")
                  ),
                  box(width = 9, title = i18n$t("Résultats"), status = "primary", solidHeader = TRUE,
                      tabsetPanel(
                        tabPanel(i18n$t("Graphique"), plotOutput("free_graph", height = "600px")),
                        tabPanel(i18n$t("Tableau"), DTOutput("free_tab"))
                      )
                  )
                )
        ),

        # ───────────────────────────────────────────────
        # Onglet 7 : Distribution des revenus
        # ───────────────────────────────────────────────
        tabItem(tabName = "distribution",
                fluidRow(
                  box(width = 12,title=i18n$t("Choix des breakdowns"),
                      uiOutput("breakdown_filters")
                  )),
                fluidRow(
                  box(width = 6,title=i18n$t("Paramètres graphiques"),
                      column(width=4,numericInput("nb_bins",i18n$t("Nombre de barres"),30,0,100)),
                      column(width=4,numericInput("revenus_min",i18n$t("Revenu min"),0)),
                      column(width=4,numericInput("revenus_max",i18n$t("Revenu max"),60000))
                  ),
                  box(width = 4,title=i18n$t("Modification de la médiane"),
                      sliderInput("modif_mediane", NULL,
                                  min = -5000, max = 5000,
                                  value = 0,step=10)
                  ),
                  box(width = 2,title=i18n$t("Pondération"),
                      radioGroupButtons("var_poids",NULL,selected = "RB050",size = "sm",
                                        choiceNames = c("Aucune","Poids Calés"),
                                        choiceValues = c("Vide","RB050"))
                      # uiOutput("ponderation_choice")
                  )
                ),
                fluidRow(
                  box(width = 12, title=i18n$t("Toute la population"),
                      plotOutput("incomePlot",height = 800))
                ),
                fluidRow(
                  box(width = 12, title=i18n$t("Zoom sur les groupes sélectionnés"),
                      plotOutput("incomePlot2",height = 800))
                )
        )
      )
    )
  ))
