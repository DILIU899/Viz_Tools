latex_to_r <- function(txt) {
  x <- trimws(txt)
  x <- gsub("\\\\left|\\\\right", "", x)
  x <- gsub("\\\\cdot|\\\\times", "*", x)
  x <- gsub("\\\\pi", "pi", x)
  x <- gsub("\\\\sin", "sin", x)
  x <- gsub("\\\\cos", "cos", x)
  x <- gsub("\\\\tan", "tan", x)
  x <- gsub("\\\\exp", "exp", x)
  x <- gsub("\\\\log", "log", x)
  x <- gsub("\\\\ln", "log", x)
  x <- gsub("\\\\abs", "abs", x)

  replace_command <- function(s, cmd, repl) {
    pattern <- paste0("\\\\", cmd, "\\{([^{}]+)\\}")
    while (grepl(pattern, s)) {
      s <- sub(pattern, paste0(repl, "(\\1)"), s)
    }
    s
  }
  x <- replace_command(x, "sqrt", "sqrt")

  frac_pattern <- "\\\\frac\\{([^{}]+)\\}\\{([^{}]+)\\}"
  while (grepl(frac_pattern, x)) {
    x <- sub(frac_pattern, "((\\1)/(\\2))", x)
  }

  x <- gsub("\\{", "(", x)
  x <- gsub("\\}", ")", x)
  x <- gsub("\\^", "^", x)
  x
}

evaluate_formula <- function(formula_text, xmin, xmax, n = 1000) {
  expr_text <- latex_to_r(formula_text)
  expr <- parse(text = expr_text)[[1]]
  allowed <- c("x", "pi", "+", "-", "*", "/", "^", "(", "sin", "cos", "tan",
               "asin", "acos", "atan", "sinh", "cosh", "tanh", "exp", "log",
               "log10", "sqrt", "abs", "floor", "ceiling", "round", "gamma",
               "lgamma", "pnorm", "dnorm")
  blocked <- setdiff(all.names(expr), allowed)
  if (length(blocked) > 0) {
    stop("Unsupported token(s): ", paste(blocked, collapse = ", "))
  }

  x <- seq(xmin, xmax, length.out = n)
  env <- list2env(list(
    x = x, pi = pi, sin = sin, cos = cos, tan = tan, asin = asin, acos = acos,
    atan = atan, sinh = sinh, cosh = cosh, tanh = tanh, exp = exp, log = log,
    log10 = log10, sqrt = sqrt, abs = abs, floor = floor, ceiling = ceiling,
    round = round, gamma = gamma, lgamma = lgamma, pnorm = pnorm, dnorm = dnorm
  ), parent = baseenv())
  y <- eval(expr, envir = env)
  if (length(y) == 1) y <- rep(y, length(x))
  data.frame(x = x, y = as.numeric(y))
}

r_code_text <- function(formula_text, xmin, xmax, n) {
  paste0(
    "x <- seq(", signif(xmin, 8), ", ", signif(xmax, 8), ", length.out = ", n, ")\n",
    "y <- ", latex_to_r(formula_text), "\n",
    "plot_data <- data.frame(x = x, y = as.numeric(y))"
  )
}
