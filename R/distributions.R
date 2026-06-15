laplace_d <- function(x, location = 0, scale = 1) {
  exp(-abs(x - location) / scale) / (2 * scale)
}

laplace_p <- function(q, location = 0, scale = 1) {
  ifelse(q < location,
    0.5 * exp((q - location) / scale),
    1 - 0.5 * exp(-(q - location) / scale)
  )
}

laplace_q <- function(p, location = 0, scale = 1) {
  ifelse(p < 0.5,
    location + scale * log(2 * p),
    location - scale * log(2 * (1 - p))
  )
}

halfnorm_d <- function(x, sigma = 1) {
  ifelse(x >= 0, sqrt(2 / pi) / sigma * exp(-x^2 / (2 * sigma^2)), 0)
}

halfnorm_p <- function(q, sigma = 1) {
  ifelse(q < 0, 0, 2 * pnorm(q / sigma) - 1)
}

halfnorm_q <- function(p, sigma = 1) {
  sigma * qnorm((p + 1) / 2)
}

dist_defs <- list(
  "Normal" = list(
    type = "continuous", params = list(mean = 0, sd = 1),
    validate = function(p) p$sd > 0,
    d = function(x, p) dnorm(x, p$mean, p$sd),
    p = function(x, p) pnorm(x, p$mean, p$sd),
    q = function(prob, p) qnorm(prob, p$mean, p$sd),
    range = function(p) qnorm(c(0.001, 0.999), p$mean, p$sd)
  ),
  "Student t" = list(
    type = "continuous", params = list(df = 8),
    validate = function(p) p$df > 0,
    d = function(x, p) dt(x, p$df),
    p = function(x, p) pt(x, p$df),
    q = function(prob, p) qt(prob, p$df),
    range = function(p) qt(c(0.001, 0.999), p$df)
  ),
  "Cauchy" = list(
    type = "continuous", params = list(location = 0, scale = 1),
    validate = function(p) p$scale > 0,
    d = function(x, p) dcauchy(x, p$location, p$scale),
    p = function(x, p) pcauchy(x, p$location, p$scale),
    q = function(prob, p) qcauchy(prob, p$location, p$scale),
    range = function(p) qcauchy(c(0.01, 0.99), p$location, p$scale)
  ),
  "Logistic" = list(
    type = "continuous", params = list(location = 0, scale = 1),
    validate = function(p) p$scale > 0,
    d = function(x, p) dlogis(x, p$location, p$scale),
    p = function(x, p) plogis(x, p$location, p$scale),
    q = function(prob, p) qlogis(prob, p$location, p$scale),
    range = function(p) qlogis(c(0.001, 0.999), p$location, p$scale)
  ),
  "Laplace" = list(
    type = "continuous", params = list(location = 0, scale = 1),
    validate = function(p) p$scale > 0,
    d = function(x, p) laplace_d(x, p$location, p$scale),
    p = function(x, p) laplace_p(x, p$location, p$scale),
    q = function(prob, p) laplace_q(prob, p$location, p$scale),
    range = function(p) laplace_q(c(0.001, 0.999), p$location, p$scale)
  ),
  "Half normal" = list(
    type = "continuous", params = list(sigma = 1),
    validate = function(p) p$sigma > 0,
    d = function(x, p) halfnorm_d(x, p$sigma),
    p = function(x, p) halfnorm_p(x, p$sigma),
    q = function(prob, p) halfnorm_q(prob, p$sigma),
    range = function(p) c(0, halfnorm_q(0.999, p$sigma))
  ),
  "Chi-square" = list(
    type = "continuous", params = list(df = 5),
    validate = function(p) p$df > 0,
    d = function(x, p) dchisq(x, p$df),
    p = function(x, p) pchisq(x, p$df),
    q = function(prob, p) qchisq(prob, p$df),
    range = function(p) c(0, qchisq(0.999, p$df))
  ),
  "F" = list(
    type = "continuous", params = list(df1 = 5, df2 = 20),
    validate = function(p) p$df1 > 0 && p$df2 > 0,
    d = function(x, p) df(x, p$df1, p$df2),
    p = function(x, p) pf(x, p$df1, p$df2),
    q = function(prob, p) qf(prob, p$df1, p$df2),
    range = function(p) c(0, qf(0.999, p$df1, p$df2))
  ),
  "Exponential" = list(
    type = "continuous", params = list(rate = 1),
    validate = function(p) p$rate > 0,
    d = function(x, p) dexp(x, p$rate),
    p = function(x, p) pexp(x, p$rate),
    q = function(prob, p) qexp(prob, p$rate),
    range = function(p) c(0, qexp(0.999, p$rate))
  ),
  "Gamma" = list(
    type = "continuous", params = list(shape = 2, rate = 1),
    validate = function(p) p$shape > 0 && p$rate > 0,
    d = function(x, p) dgamma(x, p$shape, rate = p$rate),
    p = function(x, p) pgamma(x, p$shape, rate = p$rate),
    q = function(prob, p) qgamma(prob, p$shape, rate = p$rate),
    range = function(p) c(0, qgamma(0.999, p$shape, rate = p$rate))
  ),
  "Log-normal" = list(
    type = "continuous", params = list(meanlog = 0, sdlog = 0.7),
    validate = function(p) p$sdlog > 0,
    d = function(x, p) dlnorm(x, p$meanlog, p$sdlog),
    p = function(x, p) plnorm(x, p$meanlog, p$sdlog),
    q = function(prob, p) qlnorm(prob, p$meanlog, p$sdlog),
    range = function(p) c(0, qlnorm(0.999, p$meanlog, p$sdlog))
  ),
  "Weibull" = list(
    type = "continuous", params = list(shape = 1.5, scale = 1),
    validate = function(p) p$shape > 0 && p$scale > 0,
    d = function(x, p) dweibull(x, p$shape, p$scale),
    p = function(x, p) pweibull(x, p$shape, p$scale),
    q = function(prob, p) qweibull(prob, p$shape, p$scale),
    range = function(p) c(0, qweibull(0.999, p$shape, p$scale))
  ),
  "Uniform" = list(
    type = "continuous", params = list(min = 0, max = 1),
    validate = function(p) p$max > p$min,
    d = function(x, p) dunif(x, p$min, p$max),
    p = function(x, p) punif(x, p$min, p$max),
    q = function(prob, p) qunif(prob, p$min, p$max),
    range = function(p) c(p$min, p$max)
  ),
  "Beta" = list(
    type = "continuous", params = list(shape1 = 2, shape2 = 5),
    validate = function(p) p$shape1 > 0 && p$shape2 > 0,
    d = function(x, p) dbeta(x, p$shape1, p$shape2),
    p = function(x, p) pbeta(x, p$shape1, p$shape2),
    q = function(prob, p) qbeta(prob, p$shape1, p$shape2),
    range = function(p) c(0, 1)
  ),
  "Binomial" = list(
    type = "discrete", params = list(size = 20, prob = 0.4),
    validate = function(p) p$size >= 1 && p$prob >= 0 && p$prob <= 1,
    d = function(x, p) dbinom(x, round(p$size), p$prob),
    p = function(x, p) pbinom(floor(x), round(p$size), p$prob),
    q = function(prob, p) qbinom(prob, round(p$size), p$prob),
    range = function(p) c(0, round(p$size))
  ),
  "Poisson" = list(
    type = "discrete", params = list(lambda = 5),
    validate = function(p) p$lambda > 0,
    d = function(x, p) dpois(x, p$lambda),
    p = function(x, p) ppois(floor(x), p$lambda),
    q = function(prob, p) qpois(prob, p$lambda),
    range = function(p) c(0, qpois(0.999, p$lambda))
  ),
  "Geometric" = list(
    type = "discrete", params = list(prob = 0.3),
    validate = function(p) p$prob > 0 && p$prob <= 1,
    d = function(x, p) dgeom(x, p$prob),
    p = function(x, p) pgeom(floor(x), p$prob),
    q = function(prob, p) qgeom(prob, p$prob),
    range = function(p) c(0, qgeom(0.999, p$prob))
  ),
  "Negative binomial" = list(
    type = "discrete", params = list(size = 5, prob = 0.45),
    validate = function(p) p$size > 0 && p$prob > 0 && p$prob <= 1,
    d = function(x, p) dnbinom(x, p$size, p$prob),
    p = function(x, p) pnbinom(floor(x), p$size, p$prob),
    q = function(prob, p) qnbinom(prob, p$size, p$prob),
    range = function(p) c(0, qnbinom(0.999, p$size, p$prob))
  ),
  "Hypergeometric" = list(
    type = "discrete", params = list(m = 30, n = 70, k = 20),
    validate = function(p) p$m >= 0 && p$n >= 0 && p$k >= 0 && p$k <= p$m + p$n,
    d = function(x, p) dhyper(x, round(p$m), round(p$n), round(p$k)),
    p = function(x, p) phyper(floor(x), round(p$m), round(p$n), round(p$k)),
    q = function(prob, p) qhyper(prob, round(p$m), round(p$n), round(p$k)),
    range = function(p) c(max(0, round(p$k) - round(p$n)), min(round(p$k), round(p$m)))
  ),
  "Poisson inverse Gaussian" = list(
    type = "discrete", params = list(mu = 5, sigma = 0.7),
    validate = function(p) p$mu > 0 && p$sigma > 0,
    d = function(x, p) dPIG(x, mu = p$mu, sigma = p$sigma),
    p = function(x, p) pPIG(floor(x), mu = p$mu, sigma = p$sigma),
    q = function(prob, p) qPIG(prob, mu = p$mu, sigma = p$sigma),
    range = function(p) c(0, qPIG(0.999, mu = p$mu, sigma = p$sigma))
  )
)

dist_choices <- list(
  "Continuous: location / scale" = c("Normal", "Student t", "Cauchy", "Logistic", "Laplace"),
  "Continuous: positive support" = c("Half normal", "Chi-square", "F", "Exponential", "Gamma", "Log-normal", "Weibull"),
  "Continuous: bounded support" = c("Uniform", "Beta"),
  "Discrete: count / trials" = c("Binomial", "Poisson", "Geometric", "Negative binomial", "Hypergeometric", "Poisson inverse Gaussian")
)

param_inputs <- function(dist_name) {
  defaults <- dist_defs[[dist_name]]$params
  lapply(names(defaults), function(nm) {
    step <- if (nm %in% c("size", "m", "n", "k")) 1 else 0.1
    numericInput(paste0("param_", nm), param_label(dist_name, nm), value = defaults[[nm]], step = step)
  })
}

read_params <- function(input, dist_name) {
  defaults <- dist_defs[[dist_name]]$params
  setNames(lapply(names(defaults), function(nm) input[[paste0("param_", nm)]]), names(defaults))
}
