server <- function(input, output, session) {
  output$param_ui <- renderUI(param_inputs(input$dist))

  target_visible <- reactiveVal(FALSE)
  probe_visible <- reactiveVal(FALSE)

  observeEvent(input$show_target, target_visible(TRUE))
  observeEvent(input$clear_target, target_visible(FALSE))
  observeEvent(input$show_probe, probe_visible(TRUE))
  observeEvent(input$clear_probe, probe_visible(FALSE))

  current_dist <- reactive({
    def <- dist_defs[[input$dist]]
    params <- read_params(input, input$dist)
    validate(need(all(!vapply(params, is.null, logical(1))), "Waiting for parameters."))
    params <- lapply(params, as.numeric)
    validate(need(all(vapply(params, function(x) length(x) == 1 && is.finite(x), logical(1))),
                  "Parameters must be finite numbers."))
    validate(need(isTRUE(def$validate(params)), "Invalid parameters for this distribution."))
    list(def = def, params = params)
  })

  dist_data <- reactive({
    cur <- current_dist()
    def <- cur$def
    p <- cur$params
    rng <- def$range(p)
    validate(need(all(is.finite(rng)) && diff(rng) >= 0, "Could not compute a finite plotting range."))
    if (diff(rng) == 0) rng <- rng + c(-1, 1)

    pad <- diff(rng) * 0.05
    if (def$type == "continuous") {
      x <- seq(rng[1] - pad, rng[2] + pad, length.out = input$grid_n)
    } else {
      lo <- floor(max(0, rng[1]))
      hi <- ceiling(rng[2])
      if (hi - lo > 500) hi <- lo + 500
      x <- lo:hi
    }

    y <- def$d(x, p)
    y[!is.finite(y)] <- NA_real_
    cdf <- def$p(x, p)
    value_label <- if (def$type == "continuous") "PDF" else "PMF"
    data.frame(
      x = x,
      value = y,
      cdf = cdf,
      hover = paste0(
        "<b>x</b>: ", pretty_num(x),
        "<br><b>", value_label, "</b>: ", pretty_num(y),
        "<br><b>CDF</b>: ", pretty_num(cdf)
      )
    )
  })

  target_x <- reactive({
    cur <- current_dist()
    prob <- parse_optional_number(input$target_prob)
    validate(need(is.finite(prob) && prob > 0 && prob < 1,
                  "Target probability must be strictly between 0 and 1."))
    qprob <- if (input$tail == "lower") prob else 1 - prob
    cur$def$q(qprob, cur$params)
  })

  probe_metrics <- reactive({
    cur <- current_dist()
    x <- parse_optional_number(input$probe_x)
    validate(need(is.finite(x), "Probe x must be a finite number."))
    data.frame(
      x = x,
      value = cur$def$d(x, cur$params),
      cdf = cur$def$p(x, cur$params)
    )
  })

  output$dist_plot <- renderPlotly({
    cur <- current_dist()
    dat <- dist_data()
    max_y <- max(dat$value, na.rm = TRUE)
    y_title <- if (cur$def$type == "continuous") "PDF" else "PMF"
    show_target_now <- isTRUE(target_visible())
    show_probe_now <- isTRUE(probe_visible())
    tx <- if (show_target_now) target_x() else NA_real_
    px <- if (show_probe_now) parse_optional_number(input$probe_x) else NA_real_
    validate(need(!show_probe_now || is.finite(px), "Probe x must be a finite number."))

    shade_dat <- if (show_target_now) {
      if (input$tail == "lower") dat[dat$x <= tx, ] else dat[dat$x >= tx, ]
    } else {
      dat[0, ]
    }

    p <- plot_ly()
    if (cur$def$type == "continuous") {
      p <- p |>
        add_trace(
          data = shade_dat, x = ~x, y = ~value,
          type = "scatter", mode = "lines", fill = "tozeroy",
          line = list(color = "rgba(240,68,56,0)"),
          fillcolor = "rgba(240,68,56,0.22)",
          hoverinfo = "skip", showlegend = FALSE
        ) |>
        add_lines(
          data = dat, x = ~x, y = ~value,
          hovertext = ~hover, hoverinfo = "text",
          line = list(color = "#1f77b4", width = 2),
          name = y_title
        )
    } else {
      dat$bar_color <- if (show_target_now) ifelse(dat$x %in% shade_dat$x, "#f04438", "#1f77b4") else "#1f77b4"
      p <- p |>
        add_bars(
          data = dat, x = ~x, y = ~value,
          hovertext = ~hover, hoverinfo = "text", textposition = "none",
          marker = list(color = dat$bar_color, opacity = 0.78),
          name = y_title
        )
    }

    shapes <- list()
    annotations <- list()
    if (show_target_now) {
      shapes <- append(shapes, list(list(
        type = "line", x0 = tx, x1 = tx, y0 = 0, y1 = max_y,
        xref = "x", yref = "y", line = list(color = "#d92d20", width = 3)
      )))
      annotations <- append(annotations, list(list(
        x = tx, y = max_y, xref = "x", yref = "y",
        text = paste0("<b>Target x</b><br>", pretty_num(tx)),
        showarrow = TRUE, arrowhead = 2, ax = 35, ay = -40,
        font = list(color = "#b42318", size = 14),
        bgcolor = "white", bordercolor = "#f04438", borderwidth = 1
      )))
    }
    if (show_probe_now) {
      shapes <- append(shapes, list(list(
        type = "line", x0 = px, x1 = px, y0 = 0, y1 = max_y,
        xref = "x", yref = "y", line = list(color = "#175cd3", width = 3, dash = "dash")
      )))
      annotations <- append(annotations, list(list(
        x = px, y = max_y * 0.85, xref = "x", yref = "y",
        text = paste0("<b>Probe x</b><br>", pretty_num(px)),
        showarrow = TRUE, arrowhead = 2, ax = -35, ay = -30,
        font = list(color = "#175cd3", size = 14),
        bgcolor = "white", bordercolor = "#175cd3", borderwidth = 1
      )))
    }

    p |>
      layout(
        title = list(text = input$dist, font = list(size = 18)),
        hovermode = "x",
        xaxis = list(title = "x", showspikes = TRUE, spikecolor = "#d92d20", spikethickness = 1, spikemode = "across"),
        yaxis = list(title = y_title, rangemode = "tozero"),
        bargap = 0.08,
        shapes = shapes,
        annotations = annotations
      ) |>
      config(displaylogo = FALSE)
  })

  output$target_text <- renderUI({
    if (!isTRUE(target_visible())) return(div(class = "metric-empty", "Not shown"))
    tx <- target_x()
    prob <- parse_optional_number(input$target_prob)
    tail_label <- if (input$tail == "lower") "P(X <= x)" else "P(X >= x)"
    tagList(
      div(class = "metric-line", span(class = "metric-label", "Tail"), span(class = "metric-value", tail_label)),
      div(class = "metric-line", span(class = "metric-label", "Probability"), span(class = "metric-value", pretty_num(prob))),
      div(class = "metric-line", span(class = "metric-label", "x"), span(class = "metric-value", pretty_num(tx)))
    )
  })

  output$probe_text <- renderUI({
    if (!isTRUE(probe_visible())) return(div(class = "metric-empty", "Not shown"))
    m <- probe_metrics()
    y_label <- if (current_dist()$def$type == "continuous") "PDF" else "PMF"
    tagList(
      div(class = "metric-line", span(class = "metric-label", "x"), span(class = "metric-value", pretty_num(m$x))),
      div(class = "metric-line", span(class = "metric-label", y_label), span(class = "metric-value", pretty_num(m$value))),
      div(class = "metric-line", span(class = "metric-label", "CDF"), span(class = "metric-value", pretty_num(m$cdf)))
    )
  })

  output$type_text <- renderUI({
    y_label <- if (current_dist()$def$type == "continuous") "PDF" else "PMF"
    tagList(
      div(class = "metric-line", span(class = "metric-label", "Type"), span(class = "metric-value", current_dist()$def$type)),
      div(class = "metric-line", span(class = "metric-label", "Value"), span(class = "metric-value", y_label))
    )
  })

  output$value_table <- renderDT({
    dat <- dist_data()[, c("x", "value", "cdf")]
    names(dat)[2] <- if (current_dist()$def$type == "continuous") "PDF" else "PMF"
    datatable(dat, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$dist_formula <- renderUI({
    info <- dist_info[[input$dist]]
    tagList(
      div(class = "dist-formula-main", tags$span(paste0("$$ ", info$formula, " $$"))),
      div(class = "dist-source", paste("Parameterization:", info$family, "documentation"))
    )
  })

  output$dist_stats <- renderUI({
    cur <- current_dist()
    info <- dist_info[[input$dist]]
    values <- dist_stat_values(input$dist, cur$params)
    tagList(
      span(class = "stats-title", "Theoretical statistics"),
      div(
        class = "stats-header",
        div("Formula"),
        div("Value")
      ),
      div(
        class = "stats-row",
        div(class = "stats-formula", tags$span(paste0("$$ ", info$mean_formula, " $$"))),
        div(class = "stats-value", pretty_num(values$mean))
      ),
      div(
        class = "stats-row",
        div(class = "stats-formula", tags$span(paste0("$$ ", info$var_formula, " $$"))),
        div(class = "stats-value", pretty_num(values$var))
      )
    )
  })

  fn_data <- reactive({
    validate(need(input$xmax > input$xmin, "x max must be greater than x min."))
    out <- tryCatch(
      evaluate_formula(input$formula, input$xmin, input$xmax, input$fn_n),
      error = function(e) e
    )
    validate(need(!inherits(out, "error"), out$message))
    out[is.finite(out$y), , drop = FALSE]
  })

  output$function_plot <- renderPlotly({
    dat <- fn_data()
    dat$hover <- paste0("<b>x</b>: ", pretty_num(dat$x), "<br><b>y</b>: ", pretty_num(dat$y))
    plot_ly() |>
      add_lines(
        data = dat, x = ~x, y = ~y,
        hovertext = ~hover, hoverinfo = "text",
        line = list(color = "#0f766e", width = 2),
        name = "f(x)"
      ) |>
      layout(
        title = list(text = ""),
        hovermode = "x",
        xaxis = list(title = "x", zeroline = TRUE, zerolinecolor = "#98a2b3", showspikes = TRUE, spikecolor = "#d92d20"),
        yaxis = list(title = "y", zeroline = TRUE, zerolinecolor = "#98a2b3")
      ) |>
      config(displaylogo = FALSE)
  })

  output$formula_latex <- renderUI({
    tags$span(paste0("$$ y = ", input$formula, " $$"))
  })

  output$function_r_code <- renderText({
    r_code_text(input$formula, input$xmin, input$xmax, input$fn_n)
  })
}
