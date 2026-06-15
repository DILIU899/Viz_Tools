library(shiny)
library(plotly)
library(DT)
library(gamlss.dist)

pretty_num <- function(x) {
  ifelse(is.na(x) | !is.finite(x), NA_character_, signif(x, 6))
}

parse_optional_number <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(NA_real_)
  suppressWarnings(as.numeric(x))
}
