# -----------------------------------------------------------------------------
# Code pour réutilisation
# -----------------------------------------------------------------------------

pre_code <- paste0(
  "library(fonctionr)\n",
  "\n"
)

post_code <- paste0(
  "\n\n",
  "resultats$graph\n",
  "resultats$tab"
)

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
    return(list(ok = FALSE, msg = "Invalid code: cannot be parsed."))
  }
  if (length(exprs) != 1L) {
    return(list(ok = FALSE, msg = "The code must contain exactly ONE function call"))
  }
  expr <- exprs[[1]]
  if (!is.call(expr)) {
    return(list(ok = FALSE, msg = "The code must be a function call (e.g., fun(arg1 = ..., ...))"))
  }
  # Récupère le nom de la fonction appelée
  fun_sym <- expr[[1]]
  if (!is.symbol(fun_sym)) {
    return(list(ok = FALSE, msg = "Invalid function name (no ::, :::, $, etc.)"))
  }
  fun_name <- as.character(fun_sym)
  if (!fun_name %in% allowed_funs) {
    return(list(ok = FALSE, msg = sprintf(
      "Function ‘%s’ is not allowed. Allowed: %s",
      fun_name, paste(allowed_funs, collapse = ", ")
    )))
  }
  # Vérifie les arguments (pas d'appels imbriqués, seulement symboles/NULL/scalaires)
  if (!args_are_safe(expr)) {
    return(list(ok = FALSE, msg = "Unsafe arguments: nested calls are not allowed"))
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
    msg <- i18n$t("Error in code writing")
  }else if (!expr_code$ok) {
    resultats <- NULL
    msg <- expr_code$msg
  }else{
    # 2) Évaluer en environnement isolé
    resultats <- try(eval(expr_code$expr), silent = TRUE)
    if (inherits(resultats, "try-error")) {
      msg <- paste("Error during code submitting",resultats,sep = " : ")
      resultats <- NULL
    }
  }
  list(resultats = resultats,msg = msg)
}
