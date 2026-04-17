pre_code <- paste0(
  "library(tidyverse)\n",
  "library(survey)\n",
  "load(\"Z:/E8/0514-8532-SILC/SILC-inhoudelijk/SILC 2024/Méthodologie/design_silc.RData\")\n",
  "\n",
  "design_silc_2024 <- design_silc[[design_silc_2024]]",
  "\n"
)

post_code <- paste0(
  "\n\n",
  "resultats$graph\n",
  "resultats$tab"
)

# openai_api_key(id_chatgpt)
# set_chatlog(chatlog_id = "fonctionR",initial_content = "")

breakdown_variables <- c("REGIO","DB076","RB090","CD_AGE","HT","HT2","TENSTA","ACTSTA_BE","EDUC")

prompt_fr <- readLines("prompt_fr.txt")
i18n <- Translator$new(translation_csvs_path = "i18n")
i18n$set_translation_language(i18n$get_key_translation())

prop_evo <- function(design, vc, facet = NULL, group = NULL,
                      is_prop = F) {

  group_vars <- c("RB010",facet,group)
  group_provided <- !is.null(group)
  facet_provided <- !is.null(facet)

  stat_summary <- design %>% map_df(~{.x %>%
      mutate(across(c(RB010,all_of(group_vars)),as.character)) %>%
      group_by(RB010,across(all_of(group_vars))) %>%
      summarise(
        count = n(),
        mean = srvyr::survey_mean(!!sym(vc),vartype = c("se","ci")),
        .groups = "drop"
      )
  })

  dodge_width <- if (group_provided) 0.2 else 0

  p <- ggplot(stat_summary) +
    aes(x = RB010, y = mean, group = 1)

  # Ajouter groupe et couleur si group_provided
  if (group_provided) {
    p <- p + aes(group = !!sym(group), color = !!sym(group))
  }

  p <- p +
    geom_point(position = position_dodge(width = dodge_width)) +
    geom_line(position = position_dodge(width = dodge_width), alpha = 0.5,
              linewidth=1.5) +
    geom_linerange(
      aes(ymin = mean_low, ymax = mean_upp),
      position = position_dodge(width = dodge_width),
      alpha = 0.5,linewidth=1
    ) +
    labs(x= "RB010",y = vc) +
    theme_fonctionr()

  # Facet si facet_provided
  if (facet_provided) {
    p <- p + facet_wrap(as.formula(paste("~", facet)), ncol = 1)
  }
  # Proportion si is_prop = T
  if (is_prop) {
    p <- p + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  }
  p
}

# Atomes autorisés
is_scalar_atomic <- function(x) {
  is.atomic(x) && !is.list(x) && length(x) == 1L
}
is_symbol_or_null <- function(x) is.symbol(x) || is.null(x)

# Whitelist d'appels autorisés dans les ARGUMENTS (pas au top-level)
ALLOWED_ARG_CALLS <- c("c", "list", ":")

# Validation récursive d'un argument
safe_arg <- function(x) {
  # 1) Atomique scalaire, symbole, NULL
  if (is_scalar_atomic(x) || is_symbol_or_null(x)) return(TRUE)

  # 2) pairlist (rare) : vérifier chaque élément
  if (is.pairlist(x)) {
    return(all(vapply(x, safe_arg, logical(1))))
  }

  # 3) Appel (call)
  if (is.call(x)) {
    fname_sym <- x[[1L]]
    if (!is.symbol(fname_sym)) return(FALSE)
    fname <- as.character(fname_sym)

    # Autoriser uniquement c(), list(), et ":" à l'intérieur des arguments
    if (!fname %in% ALLOWED_ARG_CALLS) return(FALSE)

    # Vérifier récursivement tous les arguments de cet appel
    args_list <- as.list(x)[-1L]
    if (length(args_list) == 0L) return(TRUE)
    return(all(vapply(args_list, safe_arg, logical(1))))
  }

  # 4) Tout le reste (langage, expressions, etc.) : interdit
  FALSE
}

# Vérifie que les arguments d'un appel top-level sont "sûrs"
args_are_safe <- function(call_expr) {
  if (length(call_expr) <= 1L) return(TRUE)
  for (i in 2:length(call_expr)) {
    arg <- call_expr[[i]]
    if (!safe_arg(arg)) return(FALSE)
  }
  TRUE
}

# Valide qu'on a exactement UNE expression et que c'est un appel à une fonction autorisée
validate_single_allowed_call <- function(code_text, allowed_funs = ALLOWED_FUNS) {
  exprs <- try(parse(text = code_text, keep.source = FALSE), silent = TRUE)
  if (inherits(exprs, "try-error")) {
    return(list(ok = FALSE, msg = "Code invalide : impossible de parser."))
  }
  if (length(exprs) != 1L) {
    return(list(ok = FALSE, msg = "Le code doit contenir exactement UN appel de fonction."))
  }
  expr <- exprs[[1]]
  if (!is.call(expr)) {
    return(list(ok = FALSE, msg = "Le code doit être un appel de fonction (ex: fun(arg1 = ..., ...))."))
  }
  # Récupère le nom de la fonction appelée
  fun_sym <- expr[[1]]
  if (!is.symbol(fun_sym)) {
    return(list(ok = FALSE, msg = "Nom de fonction invalide (pas de ::, :::, $, etc.)."))
  }
  fun_name <- as.character(fun_sym)
  if (!fun_name %in% allowed_funs) {
    return(list(ok = FALSE, msg = sprintf(
      "Fonction '%s' non autorisée. Autorisées : %s",
      fun_name, paste(allowed_funs, collapse = ", ")
    )))
  }
  # Vérifie les arguments (pas d'appels imbriqués, seulement symboles/NULL/scalaires)
  if (!args_are_safe(expr)) {
    return(list(ok = FALSE, msg = "Arguments non sûrs : appels imbriqués interdits."))
  }
  list(ok = TRUE, expr = expr, fun_name = fun_name)
}

secure_function <- function(code,allowed_funs = ALLOWED_FUNS, design){

  msg  <- ""
  design_silc <- design
  # 1) Valider structure et nom de fonction
  expr_code <- try(validate_single_allowed_call(code, allowed_funs), silent = TRUE)
  if (inherits(expr_code, "try-error")){
    resultats <- NULL
    msg <- "Erreur dans l'écriture du code."
  }else if (!expr_code$ok) {
    resultats <- NULL
    msg <- expr_code$msg
  }else{
    # 2) Évaluer en environnement isolé
    resultats <- try(eval(expr_code$expr), silent = TRUE)
    if (inherits(resultats, "try-error")) {
      resultats <- NULL
      msg <- "Erreur dans l'exécution du code."
    }
  }
  list(resultats = resultats,msg = msg)
}

pre_prompt <- function(df,langage = "fr"){

  pre_prompt_fr <- paste(
    paste(prompt_fr,collapse = " "),
    "Voici les colonnes disponible dans la base de données :",
    paste(names(df), collapse = ", ")
    )

  pre_prompt_nl <- paste(
    paste(prompt_fr,collapse = " "),
    "Voici les colonnes disponible dans la base de données :",
    paste(names(df), collapse = ", ")
  )

  if (tolower(langage) == "fr") pre_prompt_fr else pre_prompt_nl
}

ask_AI <- function(text,df,language = "fr"){
  if (language == "fr")
    out <- "Erreur. Recommencer dans quelques minutes"
  else
    out <- "Sorry"
  try({
    prompt <- paste(pre_prompt(df,language),text)
    print(prompt)
    TheOpenAIR::clear_chatlog("fonctionR")
    answer <- chat(prompt,chatlog_id = "fonctionR",
                   model = "gpt-4o-mini",output = "response_object")
    print(answer)
    out <- answer$choices$message$content
  },silent = TRUE)
  out
}

