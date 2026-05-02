server <- function(input, output, session) {

  observeEvent(input$lang, ignoreInit = TRUE, {
    i18n$set_translation_language(input$lang)
    shiny.i18n::update_lang(input$lang, session)

    updateTextInput(session,"prop_title",value = i18n$t("Proportion par groupe"))
    updateTextInput(session,"distrib_title",value = i18n$t("Distribution par groupe"))
    updateTextInput(session,"mean_title",value = i18n$t("Moyenne par groupe"))
    updateTextInput(session,"many_title",value = i18n$t("Comparaison de plusieurs indicateurs"))
    updateTextInput(session,"cont_title",value = i18n$t("Distribution d'une variable numérique"))
  })

  vars_binary <- reactive({
    obj_design$variables %>%
      select(where(is.logical) | where(~ is.numeric(.x) &&
                                         all(na.omit(unique(.x)) %in% c(0, 1)))) %>%
      names()
  })

  vars_category <- reactive({
    obj_design$variables %>%
      select(where(~ n_distinct(na.omit(.x)) < 20)) %>%
      names()
  })

  vars_numeric <- reactive({
    obj_design$variables %>%
      select(where(is.numeric)) %>%
      names()
  })

  observe({
    input$lang
    updateSelectInput(session,"prop_var",choices = vars_binary())
    updateSelectInput(session,"prop_group",choices = vars_category())
    updateSelectInput(session,"prop_fill",
                      choices = setNames(c("__none__",vars_category()),
                                         c(i18n$t("Aucun"),vars_category())))

    updateSelectInput(session,"distrib_var",choices = vars_category())
    updateSelectInput(session,"distrib_group",
                      choices = setNames(c("__none__",vars_category()),
                                         c(i18n$t("Aucun"),vars_category())))

    updateSelectInput(session,"mean_var",choices = vars_numeric())
    updateSelectInput(session,"mean_group",choices = vars_category())

    updateSelectInput(session,"cont_var",choices = vars_numeric())
    updateSelectInput(session,"cont_group",
                      choices = setNames(c("__none__",vars_category()),
                                         c(i18n$t("Aucun"),vars_category())))
  })

  #### Onglet 1 : Proportions ####
  observeEvent(input$prop_run, {
    prop_fill <- if (input$prop_fill == "__none__") NULL else as.name(input$prop_fill)
    opt_group <- if (is.null(prop_fill)) "" else paste0("  group.fill = ", input$prop_fill, ",\n")

    code <- paste0(
      "resultats <- prop_group(\n",
      "  data = obj_design,\n",
      "  prop_exp = ", input$prop_var, ",\n",
      "  group = ", input$prop_group, ",\n",
      opt_group,
      "  title = ", deparse(input$prop_title), "\n",
      ")"
    )

    code <- paste0(pre_code,code,post_code)

    resultats <- eval(substitute(
      prop_group(
        obj_design,
        group = VAR1,
        group.fill = VAR2,
        prop_exp = VAR3,
        title = input$prop_title
      ), list(VAR1 = as.name(input$prop_group),
              VAR2 = prop_fill,
              VAR3 = as.name(input$prop_var)
      )))

    min_n_sample <- min(resultats$tab$n_sample)

    output$prop_alert <- renderUI({
      if (min_n_sample < 30){
        i18n$t("⚠ Effectif insuffisant pour produire une estimation fiable pour certains groupes")
      }
    })

    output$prop_graph <- renderPlot({
      resultats$graph
    })
    output$prop_code  <- renderText(code)

    output$prop_tab <- renderDT({
      resultats$tab %>%
        mutate(
          across(starts_with("prop"), ~ round(.x * 100, 1)),
          across(starts_with("n_"), ~ round(.x, 0))
        ) %>%
        datatable(
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            dom = "Bfrtipl",
            lengthMenu = list(c(10, 20, 50, 100), c('10', '20', '50', "100")),
            pageLength = 10,
            buttons = c("copy", "csv", "excel", "pdf", "print"),
            ordering = FALSE,
            columnDefs = list(
              list(className = 'dt-right', targets = which(grepl("prop", names(resultats$tab))))
            )
          )
        ) %>%
        formatString(
          columns = names(resultats$tab)[grepl("prop", names(resultats$tab))],
          suffix = " %"
        )
    })
  })

  #### Onglet 2 : Distributions  ####
  observeEvent(input$distrib_run, {

    req(input$distrib_var != input$distrib_group)

    group <- if (input$distrib_group == "__none__") NULL else as.name(input$distrib_group)

    fun_name  <- if (is.null(group)) "distrib_d" else "distrib_group_d"
    fun <- get(fun_name)
    opt_group <- if (is.null(group)) "" else paste0("  group = ", input$distrib_group, ",\n")

    code <- paste0(
      "resultats <- ", fun_name, "(\n",
      "  data = obj_design,\n",
      "  quali_var = ", input$distrib_var, ",\n",
      opt_group,
      "  title = ", deparse(input$distrib_title), "\n",
      ")"
    )

    code <- paste0(pre_code,code,post_code)

    resultats <- eval(substitute(
      fun(
        obj_design,
        quali_var = VAR1,
        group = VAR2,
        title = input$distrib_title
      ), list(VAR1 = as.name(input$distrib_var),
              VAR2 = group)
    ))

    min_n_sample <- min(resultats$tab$n_sample)

    output$distrib_alert <- renderUI({
      if (min_n_sample < 30){
        i18n$t("⚠ Effectif insuffisant pour produire une estimation fiable pour certains groupes")
      }
    })

    output$distrib_tab   <- renderDT(resultats$tab)
    output$distrib_graph <- renderPlot(resultats$graph)
    output$distrib_code  <- renderText(code)

    output$distrib_tab <- renderDT({
      resultats$tab %>%
        mutate(across(starts_with("prop"), ~ round(.x * 100, 1))) %>%
        mutate(across(starts_with("n_"), ~ round(.x, 0))) %>%
        datatable(
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            # dom = 'tp',
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel", "pdf", "print"),
            pageLength = 10,
            ordering = FALSE,
            columnDefs = list(
              list(className = 'dt-right', targets = which(grepl("prop", names(resultats$tab))))
            )
          )
        ) %>%
        formatString(
          columns = names(resultats$tab)[grepl("prop", names(resultats$tab))],
          suffix = " %"
        )
    })

  })

  #### Onglet 3 : Moyennes   ####
  observeEvent(input$mean_run, {

    req(input$mean_var != input$mean_group)

    group <- if (input$mean_group == "__none__") NULL else as.name(input$mean_group)

    code <- paste0(
      "resultats <- mean_group(\n",
      "  data = obj_design,\n",
      "  quanti_exp = ", input$mean_var, ",\n",
      "  group = ", input$mean_group, ",\n",
      "  title = ", deparse(input$mean_title), "\n",
      ")"
    )

    code <- paste0(pre_code,code,post_code)

    resultats <- eval(substitute(
      mean_group(
        obj_design,
        quanti_exp = VAR1,
        group = VAR2,
        title = input$mean_title
      ), list(VAR1 = as.name(input$mean_var),
              VAR2 = group)
    ))

    output$mean_graph <- renderPlot(resultats$graph)
    output$mean_code  <- renderText(code)

    output$mean_tab <- renderDT({
      resultats$tab %>%
        mutate(across(starts_with("mean"), ~ round(.x, 1))) %>%
        mutate(across(starts_with("n_"), ~ round(.x, 0))) %>%
        datatable(
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            dom = "Bfrtipl",
            lengthMenu = list(c(10, 20, 50, 100), c('10', '20', '50', "100")),
            pageLength = 10,
            buttons = c("copy", "csv", "excel", "pdf", "print"),
            ordering = FALSE,
            columnDefs = list(
              list(className = 'dt-right', targets = which(grepl("prop", names(resultats$tab))))
            )
          )
        )
    })
  })

  #### Onglet 4 : Comparaisons multiples  ####
  output$many_vars_ui <- renderUI({
    pickerInput("many_vars", i18n$t("Variables à comparer :"),
                choices = sort(c(vars_numeric(),vars_binary())),
                multiple = TRUE, options = list(`actions-box` = TRUE))
  })

  observeEvent(input$many_run, {

    validate(
      need(input$many_vars, i18n$t('Check at least one variable!'))
    )

    fun <- get(input$many_type)

    code <- paste0(
      "resultats <- ",input$many_type,"(\n",
      "  data = obj_design,\n",
      "  list_vars = c(",paste(input$many_vars, collapse = ', '),"),\n",
      "  title = ", deparse(input$many_title), "\n",
      ")"
    )

    code <- paste0(pre_code,code,post_code)

    vars_syms <- lapply(input$many_vars, as.name)
    vars_call <- as.call(c(as.name("c"), vars_syms))

    resultats <- try({
      eval(substitute(
        fun(
          obj_design,
          list_vars = VAR1,
          title = input$prop_title
        ), list(VAR1 = vars_call
        )))
    },silent = T)

    if (class(resultats)[1] == "try-error"){
      output$many_alert <- renderUI({
        paste(i18n$t("⚠ Erreur dans le calcul de la fonction"),input$many_type)
      })
    }else{
      output$many_alert <- renderUI({NULL})
    }

    output$many_graph <- renderPlot(resultats$graph)
    output$many_code  <- renderText(code)

    output$many_tab <- renderDT({
      resultats$tab %>%
        mutate(across(starts_with("prop"), ~ round(.x * 100, 1))) %>%
        mutate(across(starts_with("median"), ~ round(.x, 1))) %>%
        mutate(across(starts_with("mean"), ~ round(.x, 1))) %>%
        mutate(across(starts_with("n_"), ~ round(.x, 0))) %>%
        datatable(
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            dom = "Bfrtipl",
            lengthMenu = list(c(10, 20, 50, 100), c('10', '20', '50', "100")),
            pageLength = 10,
            buttons = c("copy", "csv", "excel", "pdf", "print"),
            ordering = FALSE,
            columnDefs = list(
              list(className = 'dt-right', targets = which(grepl("prop", names(resultats$tab))))
            )
          )
        ) %>%
        formatString(
          columns = names(resultats$tab)[grepl("prop", names(resultats$tab))],
          suffix = " %"
        )
    })
  })

  #### Onglet 5 : Distribution continue  ####
  observeEvent(input$cont_run, {

    cont_group <- if (input$cont_group == "__none__") NULL else as.name(input$cont_group)
    opt_group <- if (is.null(cont_group)) "" else paste0("  group = ", input$cont_group, ",\n")

    fun_name <- if (input$cont_group == i18n$t("__none__")) "distrib_continuous" else "distrib_group_continuous"
    fun <- get(fun_name)

    code <- paste0(
      "resultats <- ",fun_name,"(\n",
      "  data = obj_design,\n",
      "  quanti_exp = ", input$cont_var, ",\n",
      opt_group,
      "  title = ", deparse(input$cont_title), "\n",
      ")"
    )

    code <- paste0(pre_code,code,post_code)

    cont_min <- max(input$cont_min,
                    min(obj_design$variables[[input$cont_var]],
                        na.rm = T))
    cont_max <- min(input$cont_max,
                    max(obj_design$variables[[input$cont_var]],
                        na.rm = T))

    resultats <- eval(substitute(
      fun(
        obj_design,
        quanti_exp = VAR1,
        group = VAR2,
        type = input$cont_type,
        bw = input$cont_bw,
        limits = c(cont_min,cont_max),
        title = input$prop_title
      ), list(VAR1 = as.name(input$cont_var),
              VAR2 = cont_group
      )))

    output$cont_graph <- renderPlot(resultats$graph)
    output$cont_code  <- renderText(code)

    output$cont_quant <- renderDT({
      resultats$quant %>%
        datatable(
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            dom = "Bfrtipl",
            lengthMenu = list(c(10, 20, 50, 100), c('10', '20', '50', "100")),
            pageLength = 10,
            buttons = c("copy", "csv", "excel", "pdf", "print"),
            ordering = FALSE,
            columnDefs = list(
              list(className = 'dt-right', targets = which(grepl("prop", names(resultats$tab))))
            )
          )
        )
    })


  })

  #### Onglet 6 : Graphique libre  ####

  free_code <- reactiveVal("")

  observeEvent(input$free_template, {

    if (input$free_template == "distrib_d"){
      code <- paste0(
        "distrib_d(\n",
        "  data = obj_design,\n",
        "  quali_var = ",vars_category()[1],",\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = \"",
        i18n$t("Distribution d'une variable catégorielle"),
        "\"\n",
        ")"
      )
    }

    if (input$free_template == "distrib_group_d"){
      code <- paste0(
        "distrib_group_d(\n",
        "  data = obj_design,\n",
        "  quali_var = ",vars_category()[1],",\n",
        "  group = ",vars_category()[2],",\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = \"",
        i18n$t("Distribution d'une variable catégorielle par groupe"),
        "\"\n",
        ")"
      )
    }

    if (input$free_template == "distrib_c"){
      code <- paste0(
        "distrib_c(\n",
        "  data = obj_design,\n",
        "  quanti_exp = ",vars_numeric()[1],",\n",
        "  type = 'median',\n",
        "  limits = NULL, # c(0,60000)\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = \"",
        i18n$t("Distribution d'une variable continue"),
        "\"\n",
        ")"
      )
    }

    if (input$free_template == "distrib_group_c"){
      code <- paste0(
        "distrib_group_c(\n",
        "  data = obj_design,\n",
        "  quanti_exp = ",vars_numeric()[1],",\n",
        "  group = ",vars_category()[1],",\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = \"",
        i18n$t("Distribution d'une variable continue par groupe"),
        "\"\n",
        ")"
      )
    }

    if (input$free_template == "prop_group"){
      code <- paste0(
        "prop_group(\n",
        "  data = obj_design,\n",
        "  prop_exp = ",vars_binary()[1],",\n",
        "  group = ",vars_category()[1],",\n",
        "  group.fill = NULL,\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = \"",
        i18n$t("Proportion par groupe"),
        "\"\n",
        ")"
      )
    }

    if (input$free_template == "central_group"){
      code <- paste0(
        "central_group(\n",
        "  data = obj_design,\n",
        "  quanti_exp = ",vars_numeric()[1],",\n",
        "  type = 'mean', #mean, median\n",
        "  group = ",vars_category()[1],",\n",
        "  group.fill = NULL,\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = \"",
        i18n$t("Valeur centrale (moyenne/médiane) par groupe"),
        "\"\n",
        ")"
      )
    }

    if (input$free_template == "many_val"){
      code <- paste0(
        "many_val(\n",
        "  data = obj_design,\n",
        "  list_vars = c(",vars_numeric()[1],", ",vars_numeric()[2],", ",vars_numeric()[3],"),\n",
        "  list_vars_lab = c('label 1', 'label 2', 'label 3'),\n",
        "  type = 'mean', #mean, median, prop\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = \"",
        i18n$t("Plusieurs indicateurs"),
        "\"\n",
        ")"
      )
    }

    if (input$free_template == "many_val_group"){
      code <- paste0(
        "many_val_group(\n",
        "  data = obj_design,\n",
        "  list_vars = c(",vars_numeric()[1],", ",vars_numeric()[2],", ",vars_numeric()[3],"),\n",
        "  list_vars_lab = c('label 1', 'label 2', 'label 3'),\n",
        "  type = 'mean', #mean, median, prop\n",
        "  group = ",vars_category()[4],",\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = \"",
        i18n$t("Plusieurs indicateurs par groupe"),
        "\"\n",
        ")"
      )
    }

    updateAceEditor(session,"free_code",value = code)
    free_code(code)
  })

  observeEvent(input$free_code, {
    free_code(input$free_code)
  })

  output$free_vars <- renderText({
    paste0("Liste des variables : ",
           paste(colnames(obj_design),collapse = ", "))
  })

  observeEvent(input$free_run, {

    print(free_code())

    code <- free_code()
    ALLOWED_FUNS <- c("many_val_group","many_val","central_group","prop_group",
                      "distrib_c","distrib_group_c","distrib_d","distrib_group_d")

    list_resultats <- secure_function(code,ALLOWED_FUNS,obj_design)
    resultats <- list_resultats$resultats
    msg <- list_resultats$msg

    output$free_graph <- renderPlot({
      validate(need(!is.null(resultats), msg))
      resultats$graph
    })

    output$free_code_output  <- renderText(paste0(pre_code,code,post_code))

    output$free_tab <- renderDT({

      resultats$tab %>%
        mutate(across(starts_with("prop"), ~ round(.x*100, 2))) %>%  # convertit en %
        mutate(across(starts_with("n_"), ~ round(.x, 0))) %>%  # convertit en %
        datatable(
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            dom = "Bfrtipl",
            lengthMenu = list(c(10, 20, 50, 100), c('10', '20', '50', "100")),
            pageLength = 10,
            buttons = c("copy", "csv", "excel", "pdf", "print"),
            ordering = FALSE,
            columnDefs = list(
              list(className = 'dt-right', targets = which(grepl("prop", names(resultats$tab))))
            )
          )
        ) %>%
        formatString(
          columns = names(resultats$tab)[grepl("prop", names(resultats$tab))],
          suffix = " %"
        )
    })


  })
}

