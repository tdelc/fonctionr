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
    updateSelectInput(session,"prop_var",choices = vars_binary())
    updateSelectInput(session,"prop_group",choices = vars_category())
    updateSelectInput(session,"prop_fill",choices = c("Aucun",vars_category()))

    updateSelectInput(session,"distrib_var",choices = vars_category())
    updateSelectInput(session,"distrib_group",choices = c("Aucun",vars_category()))

    updateSelectInput(session,"mean_var",choices = vars_numeric())
    updateSelectInput(session,"mean_group",choices = vars_category())

    updateSelectInput(session,"cont_var",choices = vars_numeric())
    updateSelectInput(session,"cont_group",choices = c("Aucun",vars_category()))
  })

  # === Onglet 1 : Proportions ===
  observeEvent(input$prop_run, {
    prop_fill <- if (input$prop_fill == "Aucun") NULL else as.name(input$prop_fill)
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

    output$prop_graph <- renderPlot({
      validate(
        need(min_n_sample > 30,"Sous groupe trop faible pour faire un graphique")
      )
      resultats$graph
    })
    output$prop_code  <- renderText(code)

    resultats$tab <- resultats$tab %>%
      select(-n_tot_weighted_low,-n_tot_weighted_upp,starts_with("n_true"))

    output$prop_low <- render_gt({
      prepa <- resultats$tab %>%
        filter(n_sample <= 10)

      if (nrow(prepa) == 0) return(NULL)

      prepa %>%
        select(-c(prop_low,prop_upp,n_tot_weighted)) %>%
        gt() %>%
        tab_header("Attention, petit échantillon détecté (<= 10)") %>%
        cols_label(n_sample = "Échantillon",prop = "Proportion") %>%
        fmt_percent(columns = starts_with("prop"),decimals = 1) %>%
        fmt_number(columns = starts_with("n"),decimals = 0,
                   sep_mark = " ")
    })

    output$prop_gt <- render_gt({
      resultats$tab %>%
        gt() %>%
        tab_header(input$prop_title) %>%
        cols_label(n_sample = "Échantillon",n_tot_weighted = "Population",
                   prop = "Proportion",
                   prop_low = "Min",prop_upp = "Max") %>%
        fmt_percent(columns = starts_with("prop"),decimals = 1) %>%
        fmt_number(columns = starts_with("n"),decimals = 0,
                   sep_mark = " ") %>%
        tab_footnote(paste0("Source : Statbel, SILC ",input$year),placement ="right")
    })

    output$prop_tab <- renderDT({
      resultats$tab %>%
        mutate(across(starts_with("prop"), ~ round(.x * 100, 1))) %>%  # convertit en %
        mutate(across(starts_with("n_"), ~ round(.x, 0))) %>%  # convertit en %
        datatable(
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel", "pdf", "print"),
            # dom = 'tp',
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

  # === Onglet 2 : Distributions ===
  observeEvent(input$distrib_run, {

    req(input$distrib_var != input$distrib_group)

    group <- if (input$distrib_group == "Aucun") NULL else as.name(input$distrib_group)

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

    output$distrib_tab   <- renderDT(resultats$tab)
    output$distrib_graph <- renderPlot(resultats$graph)
    output$distrib_code  <- renderText(code)

    output$distrib_gt <- render_gt({

      if (is.null(group)) {
        resultats$tab %>%
          gt() %>%
          tab_header(input$distrib_title) %>%
          cols_label(n_sample = "Échantillon",prop = "Proportion",
                     n_weighted = "Population",prop_low = "Min",prop_upp = "Max") %>%
          fmt_percent(columns = starts_with("prop"),decimals = 1) %>%
          fmt_number(columns = starts_with("n_"),decimals = 0,
                     sep_mark = " ") %>%
          tab_footnote(paste0("Source : Statbel, SILC ",input$year),placement ="right")

      }else{

        resultats$tab %>%
          select(!!sym(input$distrib_var),!!sym(group),prop) %>%
          pivot_wider(names_from = !!sym(group),values_from = prop) %>%
          gt() %>%
          tab_header(input$distrib_title) %>%
          fmt_percent(decimals = 1) %>%
          tab_footnote("Source : Statbel",placement ="right")
      }

    })

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

  # === Onglet 3 : Moyennes ===
  observeEvent(input$mean_run, {

    req(input$mean_var != input$mean_group)

    group <- if (input$mean_group == "Aucun") NULL else as.name(input$mean_group)

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

    output$mean_gt <- render_gt({
      resultats$tab %>%
        select(!!sym(input$mean_group),n_sample,starts_with("mean")) %>%
        gt() %>%
        tab_header(input$prop_title) %>%
        cols_label(n_sample = "Échantillon",mean = "Moyenne",
                   mean_low = "Min",mean_upp = "Max") %>%
        fmt_number(columns = starts_with("mean"),decimals = 1,
                   sep_mark = " ") %>%
        tab_footnote(paste0("Source : Statbel, SILC ",input$year),placement ="right")
    })

    output$mean_tab <- renderDT({
      resultats$tab %>%
        mutate(across(starts_with("mean"), ~ round(.x, 1))) %>%
        mutate(across(starts_with("n_"), ~ round(.x, 0))) %>%
        datatable(
          rownames = FALSE,
          options = list(
            dom = 'tp',
            pageLength = 10,
            ordering = FALSE,
            columnDefs = list(
              list(className = 'dt-right', targets = which(grepl("mean", names(resultats$tab))))
            )
          )
        )
    })
  })

  # === Onglet 4 : Comparaisons multiples ===
  output$many_vars_ui <- renderUI({
    pickerInput("many_vars", "Variables à comparer :",
                choices = names(obj_design$variables),
                multiple = TRUE, options = list(`actions-box` = TRUE))
  })

  observeEvent(input$run_many, {
    fun <- get(input$many_type)

    code <- paste0(
      "resultats <- ",input$many_type,"(\n",
      "  design_silc_", input$year, ",\n",
      "  list_vars = c(",paste(input$many_vars, collapse = ', '),"),\n",
      "  title = ", deparse(input$many_title), "\n",
      ")"
    )

    code <- paste0(pre_code,code,post_code)

    vars_syms <- lapply(input$many_vars, as.name)
    vars_call <- as.call(c(as.name("c"), vars_syms))

    resultats <- eval(substitute(
      fun(
        obj_design,
        list_vars = VAR1,
        title = input$prop_title
      ), list(VAR1 = vars_call
      )))

    output$many_graph <- renderPlot(resultats$graph)
    output$many_code  <- renderText(code)

    output$many_gt <- render_gt({
      resultats$tab %>%
        gt() %>%
        tab_header(input$distrib_title) %>%
        cols_label(n_sample = "Échantillon",n_weighted = "Population") %>%
        fmt_percent(columns = starts_with("prop"),decimals = 1) %>%
        fmt_number(columns = starts_with("median"),decimals = 1,
                   sep_mark = " ") %>%
        fmt_number(columns = starts_with("mean"),decimals = 1,
                   sep_mark = " ") %>%
        fmt_number(columns = starts_with("n_"),decimals = 0,
                   sep_mark = " ") %>%
        tab_footnote(paste0("Source : Statbel, SILC ",input$year),placement ="right")
    })

    output$many_tab <- renderDT({
      resultats$tab %>%
        mutate(across(starts_with("prop"), ~ round(.x * 100, 1))) %>%
        mutate(across(starts_with("median"), ~ round(.x, 1))) %>%
        mutate(across(starts_with("mean"), ~ round(.x, 1))) %>%
        mutate(across(starts_with("n_"), ~ round(.x, 0))) %>%
        datatable(
          rownames = FALSE,
          options = list(
            dom = 'tp',
            pageLength = 10,
            ordering = FALSE
          )
        ) %>%
        formatString(
          columns = names(resultats$tab)[grepl("prop", names(resultats$tab))],
          suffix = " %"
        )
    })
  })

  # === Onglet 5 : Distribution continue ===
  observeEvent(input$cont_run, {

    cont_group <- if (input$cont_group == "Aucun") NULL else as.name(input$cont_group)
    opt_group <- if (is.null(cont_group)) "" else paste0("  group = ", input$cont_group, ",\n")

    fun_name <- if (input$cont_group == "Aucun") "distrib_continuous" else "distrib_group_continuous"
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

    output$cont_dens <- renderDT(resultats$dens)
    output$cont_quant <- renderDT(resultats$quant)


  })

  # === Onglet 5 : Distribution continue ===
  observeEvent(input$free_template, {

    if (input$free_template == "distrib_d"){
      code <- paste0(
        "distrib_d(\n",
        "  data = obj_design,\n",
        "  quali_var = HT,\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = \"Distribution d'une variable catégorielle\"\n",
        ")"
      )
    }

    if (input$free_template == "distrib_group_d"){
      code <- paste0(
        "distrib_group_d(\n",
        "  data = obj_design,\n",
        "  quali_var = HT,\n",
        "  group = DB076,\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = \"Distribution d'une variable catégorielle par groupe\"\n",
        ")"
      )
    }

    if (input$free_template == "distrib_c"){
      code <- paste0(
        "distrib_c(\n",
        "  data = obj_design,\n",
        "  quanti_exp = EQ_INC20,\n",
        "  type = 'median',\n",
        "  limits = NULL, # c(0,60000)\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = \"Distribution d'une variable continue\"\n",
        ")"
      )
    }

    if (input$free_template == "distrib_group_c"){
      code <- paste0(
        "distrib_group_c(\n",
        "  data = obj_design,\n",
        "  quanti_exp = EQ_INC20,\n",
        "  group = RB090,\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = \"Distribution d'une variable continue par groupe\"\n",
        ")"
      )
    }

    if (input$free_template == "prop_group"){
      code <- paste0(
        "prop_group(\n",
        "  data = obj_design,\n",
        "  prop_exp = MIN60,\n",
        "  group = REGIO,\n",
        "  group.fill = NULL,\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = 'Proportion par groupe'\n",
        ")"
      )
    }

    if (input$free_template == "central_group"){
      code <- paste0(
        "central_group(\n",
        "  data = obj_design,\n",
        "  quanti_exp = EQ_INC20,\n",
        "  type = 'mean', #mean, median\n",
        "  group = REGIO,\n",
        "  group.fill = NULL,\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = 'Valeur centrale (moyenne/médiane) par groupe'\n",
        ")"
      )
    }

    if (input$free_template == "many_val"){
      code <- paste0(
        "many_val(\n",
        "  data = obj_design,\n",
        "  list_vars = c(MIN50, MIN60, MIN70),\n",
        "  list_vars_lab = c('Seuil à 50%', 'Seuil à 60%', 'Seuil à 70%'),\n",
        "  type = 'mean', #mean, median, prop\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = 'Calculer plusieurs indicateurs'\n",
        ")"
      )
    }

    if (input$free_template == "many_val_group"){
      code <- paste0(
        "many_val_group(\n",
        "  data = obj_design,\n",
        "  list_vars = c(MIN50, MIN60, MIN70),\n",
        "  list_vars_lab = c('Seuil à 50%', 'Seuil à 60%', 'Seuil à 70%'),\n",
        "  type = 'mean', #mean, median, prop\n",
        "  group = REGIO,\n",
        "  facet = NULL,\n",
        "  filter_exp = NULL,\n",
        "  title = 'Calculer plusieurs indicateurs'\n",
        ")"
      )
    }

    updateAceEditor(session,"free_code",value = code)

  })

  observeEvent(input$free_askAI,{
    try({
      text_out <- ask_AI(input$free_questionAI,obj_design$variables,"fr")
      code <- TheOpenAIR::extract_r_code(text_out)
      updateAceEditor(session,"free_code",value = code)
    },silent=TRUE)
  })

  output$free_vars <- renderText({
    paste0("Liste des variables : ",
           paste(colnames(obj_design),collapse = ", "))
  })

  observeEvent(input$free_run, {

    code <- input$free_code
    ALLOWED_FUNS <- c("many_val_group","many_val","central_group","prop_group",
                      "distrib_c","distrib_group_c","distrib_d","distrib_group_d")

    list_resultats <- secure_function(code,ALLOWED_FUNS,obj_design)
    resultats <- list_resultats$resultats
    msg <- list_resultats$msg

    output$free_graph <- renderPlot({
      validate(need(!is.null(resultats), msg))
      resultats$graph
    })

    output$free_tab <- renderDT({

      resultats$tab %>%
        mutate(across(starts_with("prop"), ~ round(.x, 4))) %>%  # convertit en %
        mutate(across(starts_with("n_"), ~ round(.x, 0))) %>%  # convertit en %
        datatable(
          rownames = FALSE
          # options = list(
          #   dom = 't',
          #   pageLength = 10,
          #   ordering = FALSE,
          #   columnDefs = list(
          #     list(className = 'dt-right', targets = which(grepl("prop", names(resultats$tab))))
          #   )
          # )
        )
      # %>%
      #   formatString(
      #     columns = names(resultats$tab)[grepl("prop", names(resultats$tab))],
      #     suffix = " %"
      #   )
    })


  })
}

