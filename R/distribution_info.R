undefined <- function(p) NA_real_

dist_info <- list(
  "Normal" = list(
    family = "stats::dnorm",
    formula = "f(x)=\\frac{1}{\\sigma\\sqrt{2\\pi}}\\exp\\left[-\\frac{(x-\\mu)^2}{2\\sigma^2}\\right]",
    params = c(mean = "\\mu", sd = "\\sigma"),
    mean_formula = "E[X]=\\mu",
    var_formula = "\\mathrm{Var}(X)=\\sigma^2",
    mean = function(p) p$mean,
    var = function(p) p$sd^2
  ),
  "Skew normal" = list(
    family = "sn::dsn",
    formula = "f(x)=\\frac{2}{\\omega}\\phi\\left(\\frac{x-\\xi}{\\omega}\\right)\\Phi\\left(\\alpha\\frac{x-\\xi}{\\omega}\\right)",
    params = c(xi = "\\xi", omega = "\\omega", alpha = "\\alpha"),
    mean_formula = "E[X]=\\xi+\\omega\\delta\\sqrt{\\frac{2}{\\pi}},\\ \\delta=\\frac{\\alpha}{\\sqrt{1+\\alpha^2}}",
    var_formula = "\\mathrm{Var}(X)=\\omega^2\\left(1-\\frac{2\\delta^2}{\\pi}\\right)",
    mean = function(p) {
      delta <- p$alpha / sqrt(1 + p$alpha^2)
      p$xi + p$omega * delta * sqrt(2 / pi)
    },
    var = function(p) {
      delta <- p$alpha / sqrt(1 + p$alpha^2)
      p$omega^2 * (1 - 2 * delta^2 / pi)
    }
  ),
  "Student t" = list(
    family = "stats::dt",
    formula = "f(x)=\\frac{\\Gamma((\\nu+1)/2)}{\\sqrt{\\nu\\pi}\\,\\Gamma(\\nu/2)}\\left(1+\\frac{x^2}{\\nu}\\right)^{-(\\nu+1)/2}",
    params = c(df = "\\nu"),
    mean_formula = "E[X]=0\\ (\\nu>1)",
    var_formula = "\\mathrm{Var}(X)=\\frac{\\nu}{\\nu-2}\\ (\\nu>2)",
    mean = function(p) if (p$df > 1) 0 else NA_real_,
    var = function(p) if (p$df > 2) p$df / (p$df - 2) else if (p$df > 1) Inf else NA_real_
  ),
  "Cauchy" = list(
    family = "stats::dcauchy",
    formula = "f(x)=\\frac{1}{\\pi s\\left[1+\\left((x-l)/s\\right)^2\\right]}",
    params = c(location = "l", scale = "s"),
    mean_formula = "E[X]\\ \\text{undefined}",
    var_formula = "\\mathrm{Var}(X)\\ \\text{undefined}",
    mean = undefined,
    var = undefined
  ),
  "Logistic" = list(
    family = "stats::dlogis",
    formula = "f(x)=\\frac{\\exp(-(x-\\mu)/s)}{s\\left(1+\\exp(-(x-\\mu)/s)\\right)^2}",
    params = c(location = "\\mu", scale = "s"),
    mean_formula = "E[X]=\\mu",
    var_formula = "\\mathrm{Var}(X)=\\frac{\\pi^2s^2}{3}",
    mean = function(p) p$location,
    var = function(p) pi^2 * p$scale^2 / 3
  ),
  "Laplace" = list(
    family = "app-defined Laplace",
    formula = "f(x)=\\frac{1}{2b}\\exp\\left(-\\frac{|x-\\mu|}{b}\\right)",
    params = c(location = "\\mu", scale = "b"),
    mean_formula = "E[X]=\\mu",
    var_formula = "\\mathrm{Var}(X)=2b^2",
    mean = function(p) p$location,
    var = function(p) 2 * p$scale^2
  ),
  "Half normal" = list(
    family = "app-defined half normal",
    formula = "f(x)=\\frac{\\sqrt{2}}{\\sigma\\sqrt{\\pi}}\\exp\\left(-\\frac{x^2}{2\\sigma^2}\\right),\\ x\\ge 0",
    params = c(sigma = "\\sigma"),
    mean_formula = "E[X]=\\sigma\\sqrt{\\frac{2}{\\pi}}",
    var_formula = "\\mathrm{Var}(X)=\\sigma^2\\left(1-\\frac{2}{\\pi}\\right)",
    mean = function(p) p$sigma * sqrt(2 / pi),
    var = function(p) p$sigma^2 * (1 - 2 / pi)
  ),
  "Chi-square" = list(
    family = "stats::dchisq",
    formula = "f(x)=\\frac{1}{2^{k/2}\\Gamma(k/2)}x^{k/2-1}\\exp(-x/2),\\ x\\ge 0",
    params = c(df = "k"),
    mean_formula = "E[X]=k",
    var_formula = "\\mathrm{Var}(X)=2k",
    mean = function(p) p$df,
    var = function(p) 2 * p$df
  ),
  "F" = list(
    family = "stats::df",
    formula = "f(x)=\\frac{(d_1/d_2)^{d_1/2}x^{d_1/2-1}}{B(d_1/2,d_2/2)\\left(1+d_1x/d_2\\right)^{(d_1+d_2)/2}},\\ x\\ge 0",
    params = c(df1 = "d_1", df2 = "d_2"),
    mean_formula = "E[X]=\\frac{d_2}{d_2-2}\\ (d_2>2)",
    var_formula = "\\mathrm{Var}(X)=\\frac{2d_2^2(d_1+d_2-2)}{d_1(d_2-2)^2(d_2-4)}\\ (d_2>4)",
    mean = function(p) if (p$df2 > 2) p$df2 / (p$df2 - 2) else NA_real_,
    var = function(p) if (p$df2 > 4) 2 * p$df2^2 * (p$df1 + p$df2 - 2) / (p$df1 * (p$df2 - 2)^2 * (p$df2 - 4)) else NA_real_
  ),
  "Exponential" = list(
    family = "stats::dexp",
    formula = "f(x)=\\lambda\\exp(-\\lambda x),\\ x\\ge 0",
    params = c(rate = "\\lambda"),
    mean_formula = "E[X]=\\frac{1}{\\lambda}",
    var_formula = "\\mathrm{Var}(X)=\\frac{1}{\\lambda^2}",
    mean = function(p) 1 / p$rate,
    var = function(p) 1 / p$rate^2
  ),
  "Gamma" = list(
    family = "stats::dgamma",
    formula = "f(x)=\\frac{\\beta^\\alpha}{\\Gamma(\\alpha)}x^{\\alpha-1}\\exp(-\\beta x),\\ x\\ge 0",
    params = c(shape = "\\alpha", rate = "\\beta"),
    mean_formula = "E[X]=\\frac{\\alpha}{\\beta}",
    var_formula = "\\mathrm{Var}(X)=\\frac{\\alpha}{\\beta^2}",
    mean = function(p) p$shape / p$rate,
    var = function(p) p$shape / p$rate^2
  ),
  "Log-normal" = list(
    family = "stats::dlnorm",
    formula = "f(x)=\\frac{1}{x\\sigma\\sqrt{2\\pi}}\\exp\\left[-\\frac{(\\log x-\\mu)^2}{2\\sigma^2}\\right],\\ x>0",
    params = c(meanlog = "\\mu", sdlog = "\\sigma"),
    mean_formula = "E[X]=\\exp\\left(\\mu+\\frac{\\sigma^2}{2}\\right)",
    var_formula = "\\mathrm{Var}(X)=\\left(\\exp(\\sigma^2)-1\\right)\\exp(2\\mu+\\sigma^2)",
    mean = function(p) exp(p$meanlog + p$sdlog^2 / 2),
    var = function(p) (exp(p$sdlog^2) - 1) * exp(2 * p$meanlog + p$sdlog^2)
  ),
  "Weibull" = list(
    family = "stats::dweibull",
    formula = "f(x)=\\frac{a}{s}\\left(\\frac{x}{s}\\right)^{a-1}\\exp\\left[-\\left(\\frac{x}{s}\\right)^a\\right],\\ x\\ge 0",
    params = c(shape = "a", scale = "s"),
    mean_formula = "E[X]=s\\Gamma\\left(1+\\frac{1}{a}\\right)",
    var_formula = "\\mathrm{Var}(X)=s^2\\left[\\Gamma\\left(1+\\frac{2}{a}\\right)-\\Gamma\\left(1+\\frac{1}{a}\\right)^2\\right]",
    mean = function(p) p$scale * gamma(1 + 1 / p$shape),
    var = function(p) p$scale^2 * (gamma(1 + 2 / p$shape) - gamma(1 + 1 / p$shape)^2)
  ),
  "Uniform" = list(
    family = "stats::dunif",
    formula = "f(x)=\\frac{1}{b-a},\\ a\\le x\\le b",
    params = c(min = "a", max = "b"),
    mean_formula = "E[X]=\\frac{a+b}{2}",
    var_formula = "\\mathrm{Var}(X)=\\frac{(b-a)^2}{12}",
    mean = function(p) (p$min + p$max) / 2,
    var = function(p) (p$max - p$min)^2 / 12
  ),
  "Beta" = list(
    family = "stats::dbeta",
    formula = "f(x)=\\frac{\\Gamma(\\alpha+\\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)}x^{\\alpha-1}(1-x)^{\\beta-1},\\ 0<x<1",
    params = c(shape1 = "\\alpha", shape2 = "\\beta"),
    mean_formula = "E[X]=\\frac{\\alpha}{\\alpha+\\beta}",
    var_formula = "\\mathrm{Var}(X)=\\frac{\\alpha\\beta}{(\\alpha+\\beta)^2(\\alpha+\\beta+1)}",
    mean = function(p) p$shape1 / (p$shape1 + p$shape2),
    var = function(p) p$shape1 * p$shape2 / ((p$shape1 + p$shape2)^2 * (p$shape1 + p$shape2 + 1))
  ),
  "Binomial" = list(
    family = "stats::dbinom",
    formula = "P(X=x)=\\binom{n}{x}p^x(1-p)^{n-x},\\ x=0,\\ldots,n",
    params = c(size = "n", prob = "p"),
    mean_formula = "E[X]=np",
    var_formula = "\\mathrm{Var}(X)=np(1-p)",
    mean = function(p) round(p$size) * p$prob,
    var = function(p) round(p$size) * p$prob * (1 - p$prob)
  ),
  "Poisson" = list(
    family = "stats::dpois",
    formula = "P(X=x)=\\frac{\\lambda^x\\exp(-\\lambda)}{x!},\\ x=0,1,2,\\ldots",
    params = c(lambda = "\\lambda"),
    mean_formula = "E[X]=\\lambda",
    var_formula = "\\mathrm{Var}(X)=\\lambda",
    mean = function(p) p$lambda,
    var = function(p) p$lambda
  ),
  "Geometric" = list(
    family = "stats::dgeom",
    formula = "P(X=x)=p(1-p)^x,\\ x=0,1,2,\\ldots",
    params = c(prob = "p"),
    mean_formula = "E[X]=\\frac{1-p}{p}",
    var_formula = "\\mathrm{Var}(X)=\\frac{1-p}{p^2}",
    mean = function(p) (1 - p$prob) / p$prob,
    var = function(p) (1 - p$prob) / p$prob^2
  ),
  "Negative binomial" = list(
    family = "stats::dnbinom",
    formula = "P(X=x)=\\frac{\\Gamma(x+r)}{\\Gamma(r)x!}p^r(1-p)^x,\\ x=0,1,2,\\ldots",
    params = c(size = "r", prob = "p"),
    mean_formula = "E[X]=\\frac{r(1-p)}{p}",
    var_formula = "\\mathrm{Var}(X)=\\frac{r(1-p)}{p^2}",
    mean = function(p) p$size * (1 - p$prob) / p$prob,
    var = function(p) p$size * (1 - p$prob) / p$prob^2
  ),
  "Hypergeometric" = list(
    family = "stats::dhyper",
    formula = "P(X=x)=\\frac{\\binom{m}{x}\\binom{n}{k-x}}{\\binom{m+n}{k}}",
    params = c(m = "m", n = "n", k = "k"),
    mean_formula = "E[X]=k\\frac{m}{m+n}",
    var_formula = "\\mathrm{Var}(X)=k\\frac{mn}{(m+n)^2}\\frac{m+n-k}{m+n-1}",
    mean = function(p) p$k * p$m / (p$m + p$n),
    var = function(p) p$k * p$m * p$n * (p$m + p$n - p$k) / ((p$m + p$n)^2 * (p$m + p$n - 1))
  ),
  "Poisson inverse Gaussian" = list(
    family = "gamlss.dist::dPIG",
    formula = "P(Y=y)=\\left(\\frac{2\\alpha}{\\pi}\\right)^{1/2}\\frac{\\mu^y\\exp(1/\\sigma)K_y(\\alpha)}{(\\alpha\\sigma)^y y!},\\ \\alpha^2=\\frac{1}{\\sigma^2}+\\frac{2\\mu}{\\sigma}",
    params = c(mu = "\\mu", sigma = "\\sigma"),
    mean_formula = "E[Y]=\\mu",
    var_formula = "\\mathrm{Var}(Y)=\\mu+\\sigma\\mu^2",
    mean = function(p) p$mu,
    var = function(p) p$mu + p$sigma * p$mu^2
  )
)

dist_param_html <- function(dist_name) {
  info <- dist_info[[dist_name]]
  tags$span(
    lapply(names(info$params), function(nm) {
      tags$span(class = "param-map", paste0(nm, " ($", info$params[[nm]], "$)"))
    })
  )
}

latex_symbol_to_label <- function(x) {
  map <- c(
    "\\mu" = "μ",
    "\\sigma" = "σ",
    "\\nu" = "ν",
    "\\lambda" = "λ",
    "\\alpha" = "α",
    "\\beta" = "β",
    "\\xi" = "ξ",
    "\\omega" = "ω"
  )
  if (x %in% names(map)) return(unname(map[[x]]))
  x
}

param_label <- function(dist_name, param_name) {
  info <- dist_info[[dist_name]]
  symbol <- info$params[[param_name]]
  if (is.null(symbol) || is.na(symbol)) return(param_name)
  paste0(param_name, " (", latex_symbol_to_label(symbol), ")")
}

dist_stat_values <- function(dist_name, params) {
  info <- dist_info[[dist_name]]
  list(mean = info$mean(params), var = info$var(params))
}
