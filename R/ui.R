app_css <- "
body { background: #f7f8fa; }
.navbar { border-radius: 0; }
.well { background: transparent; border: 0; box-shadow: none; padding: 0; }
.control-card { background: #f5f7fa; border: 1px solid #d0d5dd; border-radius: 8px; padding: 16px; margin-bottom: 14px; }
.control-label { font-weight: 600; }
.section-title { margin: 20px 0 12px; padding: 12px 0 0; border-top: 2px solid #98a2b3; font-size: 15px; font-weight: 900; color: #101828; text-transform: uppercase; letter-spacing: .06em; }
.section-title:first-child { margin-top: 0; padding-top: 0; border-top: 0; }
.help-note { color: #344054; font-size: 13px; line-height: 1.45; background: #eef4ff; border: 1px solid #b2ccff; border-left: 4px solid #175cd3; border-radius: 8px; padding: 10px 12px; margin: 10px 0 14px; }
.inline-buttons { display: flex; gap: 8px; margin: 6px 0 10px; }
.inline-buttons .btn { flex: 1; font-weight: 700; }
details.advanced { margin: 18px 0 14px; padding: 12px 0 0; border-top: 2px solid #98a2b3; }
details.advanced summary { cursor: pointer; font-size: 15px; font-weight: 900; color: #101828; text-transform: uppercase; letter-spacing: .06em; }
.metric { min-height: 112px; padding: 12px 14px; background: white; border: 1px solid #e4e7ec; border-radius: 8px; margin-bottom: 8px; }
.metric-title { display: block; color: #344054; font-size: 15px; font-weight: 800; margin-bottom: 8px; }
.metric-line { display: flex; justify-content: space-between; gap: 14px; border-top: 1px solid #f2f4f7; padding-top: 6px; margin-top: 6px; font-size: 15px; }
.metric-line:first-child { border-top: 0; padding-top: 0; margin-top: 0; }
.metric-label { color: #667085; font-weight: 800; }
.metric-value { color: #101828; font-weight: 800; font-size: 16px; text-align: right; }
.metric-empty { color: #98a2b3; font-size: 14px; }
.formula-preview { background: white; border: 1px solid #d0d5dd; border-radius: 8px; padding: 14px 18px; margin-bottom: 12px; font-size: 22px; overflow-x: auto; }
.code-preview { background: #101828; color: #f9fafb; border-radius: 8px; padding: 14px 16px; margin-top: 12px; font-size: 13px; line-height: 1.5; overflow-x: auto; }
.code-preview code { color: #f9fafb; background: transparent; padding: 0; white-space: pre; }
textarea.form-control { font-family: Menlo, Consolas, monospace; resize: vertical; min-height: 110px; }
"

mathjax_refresh_js <- "
$(document).on('shiny:value', function(event) {
  if (event.name !== 'formula_latex') return;
  setTimeout(function() {
    var el = document.getElementById('formula_latex');
    if (!el || !window.MathJax) return;
    if (window.MathJax.typesetPromise) {
      window.MathJax.typesetPromise([el]);
    } else if (window.MathJax.Hub) {
      window.MathJax.Hub.Queue(['Typeset', window.MathJax.Hub, el]);
    }
  }, 50);
});
"

distribution_sidebar <- function() {
  sidebarPanel(
    width = 3,
    div(
      class = "control-card",
      div(class = "section-title", "Distribution"),
      selectInput("dist", "Distribution", choices = dist_choices, selected = "Normal"),
      uiOutput("param_ui"),
      div(class = "section-title", "Probability marker"),
      tags$label(class = "control-label", "Tail definition"),
      radioButtons("tail", NULL, c("P(X <= x)" = "lower", "P(X >= x)" = "upper")),
      textInput("target_prob", "Target probability", value = "", placeholder = "e.g. 0.95"),
      div(class = "inline-buttons",
        actionButton("show_target", "Show", class = "btn-primary"),
        actionButton("clear_target", "Clear")
      ),
      div(class = "section-title", "X marker"),
      textInput("probe_x", "Probe x", value = "", placeholder = "e.g. 0"),
      div(class = "inline-buttons",
        actionButton("show_probe", "Show", class = "btn-primary"),
        actionButton("clear_probe", "Clear")
      ),
      tags$details(
        class = "advanced",
        tags$summary("ADVANCED CONTROLS"),
        sliderInput("grid_n", "Resolution", min = 200, max = 3000, value = 1000, step = 100),
        checkboxInput("show_table", "Show value table", FALSE)
      )
    ),
    div(class = "help-note",
      tags$b("Hover anywhere on the plot"),
      " to read x, CDF, and the current ",
      tags$b("PDF"),
      " or ",
      tags$b("PMF"),
      ". Use the marker sections to add or clear the red probability marker and blue x marker."
    )
  )
}

function_sidebar <- function() {
  sidebarPanel(
    width = 3,
    div(
      class = "control-card",
      textAreaInput("formula", "y = f(x)", value = "\\sin(x) + \\frac{x^2}{10}", rows = 5),
      numericInput("xmin", "x min", value = -10, step = 1),
      numericInput("xmax", "x max", value = 10, step = 1),
      tags$details(
        class = "advanced",
        tags$summary("ADVANCED CONTROLS"),
        sliderInput("fn_n", "Resolution", min = 200, max = 5000, value = 1200, step = 100),
        checkboxInput("show_r_code", "Show R code", FALSE)
      )
    ),
    div(class = "help-note",
      tags$b("Formula input supports LaTeX-style syntax"),
      " such as \\sin(x), \\sqrt{x}, \\frac{x}{1+x^2}, powers with ^, plus R-style functions like exp(x) or log(x)."
    )
  )
}

ui <- navbarPage(
  "Useful Tools",
  header = tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML(mathjax_refresh_js))
  ),
  tabPanel(
    "Distributions",
    sidebarLayout(
      distribution_sidebar(),
      mainPanel(
        width = 9,
        plotlyOutput("dist_plot", height = "620px"),
        fluidRow(
          column(4, div(class = "metric", span(class = "metric-title", "Target quantile"), uiOutput("target_text"))),
          column(4, div(class = "metric", span(class = "metric-title", "Probe value"), uiOutput("probe_text"))),
          column(4, div(class = "metric", span(class = "metric-title", "Distribution"), uiOutput("type_text")))
        ),
        conditionalPanel("input.show_table", DTOutput("value_table"))
      )
    )
  ),
  tabPanel(
    "Function shape",
    sidebarLayout(
      function_sidebar(),
      mainPanel(
        width = 9,
        withMathJax(div(class = "formula-preview", uiOutput("formula_latex"))),
        plotlyOutput("function_plot", height = "620px"),
        conditionalPanel("input.show_r_code", div(class = "code-preview", tags$code(textOutput("function_r_code"))))
      )
    )
  )
)
