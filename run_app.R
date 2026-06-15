required_packages <- c(
  "shiny",
  "plotly",
  "DT",
  "gamlss.dist"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

shiny::runApp(".", launch.browser = TRUE)
