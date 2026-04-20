# -----------------------------------------------------------------------------
# 1. UI : tableau de bord
# -----------------------------------------------------------------------------
i18n$use_js()

ui <- page_navbar(
  id = "main_nav",
  title = "FonctionR",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
    # primary = "#2C3E50",
    # "navbar-bg" = "#2C3E50"
  ),
  lang = "fr",
  header = tagList(
    usei18n(i18n)
  ),

  # ───────────────────────────────────────────────
  # Onglet 1 : Proportions
  # ───────────────────────────────────────────────
  nav_panel(
    title = i18n$t("Proportions"),
    icon = bs_icon("pie-chart"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        div(i18n$t("Paramètres"), class = "sidebar-title"),
        selectInput("prop_var", i18n$t("Variable à expliquer (binaires) :"), choices = NULL),
        selectInput("prop_group", i18n$t("Variable de groupe :"), choices = NULL),
        selectInput("prop_fill", i18n$t("Sous-groupe (optionnel) :"), choices = c("Aucun")),
        textInput("prop_title", i18n$t("Titre du graphique :"), "Proportion par groupe"),
        actionButton("prop_run", i18n$t("Exécuter"), icon = icon("play"), class = "btn-success")
      ),
      navset_card_tab(
        nav_panel(i18n$t("Graphique"), plotOutput("prop_graph", height = "600px")),
        nav_panel(i18n$t("Tableau"), DTOutput("prop_tab")),
        nav_panel(i18n$t("Tableau de synthèse"), gt_output("prop_gt")),
        nav_panel(i18n$t("Code R"), verbatimTextOutput("prop_code"))
      )
    )
  ),

  # ───────────────────────────────────────────────
  # Onglet 2 : Distributions
  # ───────────────────────────────────────────────
  nav_panel(
    title = i18n$t("Distributions"),
    icon = bs_icon("list"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        div(i18n$t("Paramètres"), class = "sidebar-title"),
        selectInput("distrib_var", i18n$t("Variable catégorielle :"), choices = NULL),
        selectInput("distrib_group", i18n$t("Groupe (optionnel) :"), choices = c("Aucun")),
        textInput("distrib_title", i18n$t("Titre :"), "Distribution par groupe"),
        actionButton("distrib_run", i18n$t("Exécuter"), icon = icon("play"), class = "btn-success")
      ),
      navset_card_tab(
        nav_panel(i18n$t("Graphique"), plotOutput("distrib_graph", height = "600px")),
        nav_panel(i18n$t("Tableau"), DTOutput("distrib_tab")),
        nav_panel(i18n$t("Tableau de synthèse"), gt_output("distrib_gt")),
        nav_panel(i18n$t("Code R"), verbatimTextOutput("distrib_code"))
      )
    )
  ),

  # ───────────────────────────────────────────────
  # Onglet 3 : Moyennes
  # ───────────────────────────────────────────────
  nav_panel(
    title = i18n$t("Moyennes"),
    icon = bs_icon("graph-up"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        div(i18n$t("Paramètres"), class = "sidebar-title"),
        selectInput("mean_var", i18n$t("Variable quantitative :"), choices = NULL),
        selectInput("mean_group", i18n$t("Groupe :"), choices = c(NULL)),
        textInput("mean_title", i18n$t("Titre :"), "Moyenne par groupe"),
        actionButton("mean_run", i18n$t("Exécuter"), icon = icon("play"), class = "btn-success")
      ),
      navset_card_tab(
        nav_panel(i18n$t("Graphique"), plotOutput("mean_graph", height = "600px")),
        nav_panel(i18n$t("Tableau"), DTOutput("mean_tab")),
        nav_panel(i18n$t("Tableau de synthèse"), gt_output("mean_gt")),
        nav_panel(i18n$t("Code R"), verbatimTextOutput("mean_code"))
      )
    )
  ),

  # ───────────────────────────────────────────────
  # Onglet 4 : Comparaisons multiples
  # ───────────────────────────────────────────────
  nav_panel(
    title = i18n$t("Comparaisons multiples"),
    icon = bs_icon("layers"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        div(i18n$t("Paramètres"), class = "sidebar-title"),
        uiOutput("many_vars_ui"),
        selectInput("many_type", i18n$t("Type de calcul :"),
                    choices = c("Mean" = "many_mean",
                                "Median" = "many_median",
                                "Proportion" = "many_prop")),
        textInput("many_title", i18n$t("Titre :"), "Comparaison de plusieurs indicateurs"),
        actionButton("many_run", i18n$t("Exécuter"), icon = icon("play"), class = "btn-success")
      ),
      navset_card_tab(
        nav_panel(i18n$t("Graphique"), plotOutput("many_graph", height = "600px")),
        nav_panel(i18n$t("Tableau"), DTOutput("many_tab")),
        nav_panel(i18n$t("Tableau de synthèse"), gt_output("many_gt")),
        nav_panel(i18n$t("Code R"), verbatimTextOutput("many_code"))
      )
    )
  ),

  # ───────────────────────────────────────────────
  # Onglet 5 : Distribution continue
  # ───────────────────────────────────────────────
  nav_panel(
    title = i18n$t("Distributions continues"),
    icon = bs_icon("soundwave"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        div(i18n$t("Paramètres"), class = "sidebar-title"),
        selectInput("cont_var", i18n$t("Variable continue :"), choices = NULL),
        selectInput("cont_group", i18n$t("Groupe (optionnel) :"), choices = c("Aucun")),
        selectInput("cont_type", i18n$t("Type :"), choices = c("mean", "median")),
        numericInput("cont_bw", i18n$t("Largeur de bande (bw)"), value = 0.7, min = 0.1, max = 2, step = 0.1),
        numericInput("cont_min", i18n$t("Minimum"), value = 0),
        numericInput("cont_max", i18n$t("Maximum"), value = 100000),
        textInput("cont_title", i18n$t("Titre :"), "Distribution d'une variable numérique"),
        actionButton("cont_run", i18n$t("Exécuter"), icon = icon("play"), class = "btn-success")
      ),
      navset_card_tab(
        nav_panel(i18n$t("Graphique"), plotOutput("cont_graph", height = "600px")),
        nav_panel(i18n$t("Tableau des quantiles"), DTOutput("cont_quant")),
        nav_panel(i18n$t("Données de densité"), DTOutput("cont_dens")),
        nav_panel(i18n$t("Code R"), verbatimTextOutput("cont_code"))
      )
    )
  ),

  # ───────────────────────────────────────────────
  # Onglet 6 : Graphique libre
  # ───────────────────────────────────────────────
  nav_panel(
    title = i18n$t("Graphique libre"),
    icon = bs_icon("lightbulb"),
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        radioGroupButtons(
          inputId = "free_template",
          label = "Choisir un template",
          choiceNames = list(i18n$t("Distribution catégorielle"),
                             i18n$t("Distribution catégorielle par groupe"),
                             i18n$t("Distribution continue"),
                             i18n$t("Distribution continue par groupe"),
                             i18n$t("Proportion par groupe"),
                             i18n$t("Stat par groupe"),
                             i18n$t("Plusieurs indicateurs"),
                             i18n$t("Plusieurs indicateurs par groupe")),
          choiceValues = list("distrib_d",
                              "distrib_group_d",
                              "distrib_c",
                              "distrib_group_c",
                              "prop_group",
                              "central_group",
                              "many_val",
                              "many_val_group"),
          individual = TRUE, justified = FALSE, size = "xs"
        ),
        div(i18n$t("Code"), class = "sidebar-title"),
        aceEditor(
          outputId = "free_code",
          value = "",
          mode = "r",
          theme = "chrome",
          height = "200px",
          showLineNumbers = FALSE,
          wordWrap = TRUE,
          fontSize = 14
        ),
        actionButton("free_run", i18n$t("Exécuter le code"), icon = icon("play"), class = "btn-success"),
        textOutput("free_vars")
      ),
      navset_card_tab(
        nav_panel(i18n$t("Graphique"), plotOutput("free_graph", height = "600px")),
        nav_panel(i18n$t("Tableau"), DTOutput("free_tab"))
      )
    )
  ),

  nav_spacer(),  # pousse les éléments suivants à droite

  nav_item(
    selectInput(
      inputId='lang', label=NULL,
      choices = i18n$get_languages(),
      selected = i18n$get_key_translation()
    )
  )
)
