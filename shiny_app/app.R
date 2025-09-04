suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(lubridate)
  library(scales)
  library(shiny)
  library(shinyWidgets)
})

ui <- fluidPage(
  titlePanel("Portfolio Growth with Monte Carlo Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      autonumericInput(
        inputId = "start_val",
        label   = "Initial Capital [€]",
        value   = 10000,
        currencySymbol = " €",
        currencySymbolPlacement = "s",
        decimalCharacter = ",",
        digitGroupSeparator = ".",
        decimalPlaces = 0
      ),
      autonumericInput(
        inputId = "m_save",
        label   = "Monthly Savings [€]",
        value   = 500,
        currencySymbol = " €",
        currencySymbolPlacement = "s",
        decimalCharacter = ",",
        digitGroupSeparator = ".",
        decimalPlaces = 0
      ),
      
      sliderInput("mu", "Annual Return (μ, p.a.)",
                  min = 0, max = 40, value = 8, step = 0.5, post = "%"),
      sliderInput("sigma", "Volatility (σ, p.a.)",
                  min = 0, max = 30, value = 18, step = 1, post = "%"),
      
      autonumericInput(
        inputId = "elastic",
        label   = "Savings Elasticity (Δ €/month)",
        value   = 200,
        currencySymbol = " €",
        currencySymbolPlacement = "s",
        decimalCharacter = ",",
        digitGroupSeparator = ".",
        decimalPlaces = 0
      ),
      
      checkboxInput("stresstest", "Stress Test (halve μ, double σ across horizon)", value = FALSE),
      
      sliderInput("horizon_years", "Time Horizon [years]", min = 1, max = 30, value = 15, step = 1),
      sliderInput("sims", "Number of Simulations", min = 1000, max = 9000, value = 3000, step = 1000),
      
      br(),
      uiOutput("sigma_explain_box")
    ),
    
    mainPanel(
      plotOutput("fanPlot", height = "560px"),
      plotOutput("endDist", height = "420px"),
      tags$hr(),
      fluidRow(
        column(
          6,
          h4("Target Probability"),
          autonumericInput(
            inputId = "goal_value",
            label   = "Target Value [€]",
            value   = 100000,
            currencySymbol = " €",
            currencySymbolPlacement = "s",
            decimalCharacter = ",",
            digitGroupSeparator = ".",
            decimalPlaces = 0
          ),
          dateInput("goal_date", "Target Date", value = Sys.Date() %m+% years(10), format  = "dd-mm-yyyy"),
          textOutput("goalprob"),
        ),
        column(
          6,
          h4("Savings Elasticity (Horizon Uplift)"),
          verbatimTextOutput("uplift_summary")
        )
      )
    )
  )
)

server <- function(input, output, session){
  
  # ---- Simulation Helper ----
  simulate_paths <- function(n_sims, months, mu_a, sig_a, start_val, m_sav){
    set.seed(42)
    mat <- matrix(NA_real_, nrow = months, ncol = n_sims)
    for (s in 1:n_sims) {
      v <- start_val
      for (m in 1:months) {
        r_m <- rnorm(1, mean = mu_a/12, sd = sig_a/sqrt(12))
        v <- (v + m_sav) * (1 + r_m)
        mat[m, s] <- v
      }
    }
    mat
  }
  
  # ---- Reaktive Fan-Chart Daten ----
  fan_data <- reactive({
    months   <- input$horizon_years * 12L
    dates    <- seq(from = floor_date(Sys.Date(), "month") %m+% months(1),
                    by = "month", length.out = months)
    
    mu_a    <- input$mu / 100
    sigma_a <- input$sigma / 100
    
    if (isTRUE(input$stresstest)) {
      mu_a    <- mu_a / 2
      sigma_a <- sigma_a * 2
    }
    
    start_val <- input$start_val
    m_save    <- input$m_save
    
    # Deterministischer Erwartungspfad
    v <- start_val; det <- numeric(months)
    for (m in 1:months) { v <- (v + m_save) * (1 + mu_a/12); det[m] <- v }
    det_df <- data.frame(Datum = dates, Depot = det)
    
    # Monte-Carlo
    sim_mat <- simulate_paths(input$sims, months, mu_a, sigma_a, start_val, m_save)
    
    quant_df <- data.frame(
      Datum = dates,
      p05 = apply(sim_mat, 1, quantile, 0.05),
      p25 = apply(sim_mat, 1, quantile, 0.25),
      p50 = apply(sim_mat, 1, quantile, 0.50),
      p75 = apply(sim_mat, 1, quantile, 0.75),
      p95 = apply(sim_mat, 1, quantile, 0.95)
    )
    
    list(det_df=det_df, quant_df=quant_df, sim_mat=sim_mat,
         mu_a=mu_a, sigma_a=sigma_a, months=months, dates=dates)
  })
  
  # ---- Fan Chart ----
  output$fanPlot <- renderPlot({
    fd <- fan_data()
    det_df <- fd$det_df; quant_df <- fd$quant_df
    
    median_lab <- quant_df %>% tail(1) %>% transform(y = p50, label="Median path")
    det_lab    <- det_df %>% tail(1) %>% transform(y = Depot, label="Expectation path")
    
    # Zielwert Linien
    zielwert <- as.numeric(input$goal_value)
    zieldatum <- input$goal_date
    
    ggplot(quant_df, aes(Datum, p50)) +
      geom_ribbon(aes(ymin = p05, ymax = p95), fill="grey70", alpha = 0.12) +
      geom_ribbon(aes(ymin = p25, ymax = p75), fill="grey40", alpha = 0.25) +
      geom_line(linewidth=1, color="black") +
      geom_line(data=det_df, aes(Datum, Depot), linetype="dashed", color="blue") +
      geom_text_repel(data = median_lab, aes(x=Datum, y=y, label=label),
                      nudge_x=60, color="black", size=6, fontface="bold") +
      geom_text_repel(data = det_lab, aes(x=Datum, y=y, label=label),
                      nudge_x=60, color="blue", size=6, fontface="bold") +
      # Zielwert horizontale Linie
      geom_hline(yintercept = zielwert, linetype="dashed", linewidth=1, color="lightgreen") +
      # Zieldatum vertikale Linie
      geom_vline(xintercept = as.numeric(zieldatum), linetype="dashed", linewidth=1, color="lightgreen") +
      labs(
        title = paste0("Portfolio value over ", input$horizon_years, " years"),
        x = "", 
        y = "Portfolio Value [ € ]"
      ) +
      scale_y_continuous(labels=scales::label_number(big.mark=".", decimal.mark=",")) +
      theme_minimal() +
      theme(
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        plot.title   = element_text(size = 18, face = "bold", hjust = 0.5)
      )
  })

  # ---- Endwert-Verteilung (Glockenkurve) ----
  output$endDist <- renderPlot({
    fd <- fan_data()
    months   <- fd$months
    sim_mat  <- fd$sim_mat
    
    # Endwerte aller Simulationen am Horizont
    end_vals <- sim_mat[months, ]
    
    # Kenngrößen & Perzentile
    q05 <- as.numeric(quantile(end_vals, 0.05))
    q25 <- as.numeric(quantile(end_vals, 0.25))
    q50 <- as.numeric(quantile(end_vals, 0.50))  # Median
    q75 <- as.numeric(quantile(end_vals, 0.75))
    q95 <- as.numeric(quantile(end_vals, 0.95))
    muE <- mean(end_vals)                        # Erwartungswert
    goal_v <- as.numeric(input$goal_value)       # Zielwert für grüne Linie
    
    # Dichte schätzen
    dens <- density(end_vals, n = 512, adjust = 1)
    ddf  <- data.frame(x = dens$x, y = dens$y)
    
    # (3) Normieren: Maximalwert der Glocke = 1
    y_max <- max(ddf$y)
    ddf$y_norm <- ddf$y / y_max
    
    # (4) Rechts sinnvoll abschneiden (an Fan-Chart-Skalierung orientiert):
    #      bis etwas über das 95%-Perzentil bzw. Zielwert
    x_max <- max(q95, goal_v) * 1.3
    ddf_trunc <- subset(ddf, x >= 0 & x <= x_max)
    
    # (1) Labels vertikal versetzen
    lab_df <- data.frame(
      x     = c(q50, muE),
      y     = c(0.90, 0.72),           # vertikal versetzt (normierte Skala)
      label = c("Median", "Mean"),
      col   = c("black", "blue")
    )
    
    ggplot(ddf_trunc, aes(x = x, y = y_norm)) +
      # hellgrauer Bereich: 5–95 %
      geom_area(data = subset(ddf_trunc, x >= q05 & x <= q95),
                aes(x = x, y = y_norm),
                fill = "grey70", alpha = 0.12) +
      # dunkelgrauer Bereich: 25–75 %
      geom_area(data = subset(ddf_trunc, x >= q25 & x <= q75),
                aes(x = x, y = y_norm),
                fill = "grey40", alpha = 0.25) +
      # Dichtekurve (normiert)
      geom_line(linewidth = 1, color = "black") +
      # (1) Median (schwarz) und Erwartungswert (blau, gestrichelt)
      geom_vline(xintercept = q50, color = "black", linewidth = 1) +
      geom_vline(xintercept = muE, color = "blue", linetype = "dashed", linewidth = 1) +
      # (2) Zielwert: vertikale, gestrichelte, hellgrüne Linie
      geom_vline(xintercept = goal_v, color = "lightgreen", linetype = "dashed", linewidth = 1) +
      # Labels mit ggrepel (nicht überlappend, vertikal versetzt)
      ggrepel::geom_text_repel(
        data = lab_df,
        aes(x = x, y = y, label = label, color = I(col)),
        nudge_x = (x_max - 0) * 0.06,
        direction = "x",
        segment.alpha = 0.5,
        size = 5,
        fontface = "bold",
        show.legend = FALSE
      ) +
      labs(
        title = paste0("Distribution of the End Value after ", input$horizon_years, " years"),
        x = "End Value [ € ]",
        y = "Density (normalized)"
      ) +
      scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","), limits = c(0, x_max)) +
      # (3) y-Achse gut lesbar (0..1, keine wissenschaftliche Notation)
      scale_y_continuous(labels = scales::label_number(accuracy = 0.1, decimal.mark = ","), limits = c(0, 1.05)) +
      theme_minimal() +
      theme(
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title   = element_text(size = 18, face = "bold", hjust = 0.5)
      )
  })
  
  # ---- Zielwahrscheinlichkeit ----
  output$goalprob <- renderText({
    fd <- fan_data()
    sim_mat <- fd$sim_mat
    dates   <- fd$dates
    goal_v  <- as.numeric(input$goal_value)
    goal_d  <- input$goal_date
    
    idx <- which.min(abs(dates - goal_d))
    if (length(idx)==0 || idx > nrow(sim_mat)) return("Date outside the horizon")
    
    prob <- mean(sim_mat[idx, ] >= goal_v)
    
    paste0(
      "Probability that on ",
      format(goal_d, "%d-%m-%Y"),
      " at least ",
      formatC(goal_v, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
      " € is reached: ",
      scales::percent(prob, accuracy = 0.1, decimal.mark = ",")
    )
  })
  
  # ---- Sparraten-Elastizität ----
  output$uplift_summary <- renderText({
    fd <- fan_data()
    months <- fd$months
    sim_base <- fd$sim_mat
    sim_up   <- simulate_paths(input$sims, months,
                               fd$mu_a, fd$sigma_a,
                               input$start_val, input$m_save + input$elastic)
    
    base_end <- median(sim_base[months, ])
    up_end   <- median(sim_up[months, ])
    uplift   <- up_end - base_end
    
    paste0("If the monthly savings increases by ", input$elastic, " €,\n",
           "the median end value after ", input$horizon_years, " years\n",
           "is higher by approx. ", format(round(uplift,0), big.mark=".", decimal.mark=","), " €.")
  })
  
  # ---- Dynamische σ-Erklärung ----
  output$sigma_explain_box <- renderUI({
    mu    <- input$mu/100
    sigma <- input$sigma/100
    rng68 <- paste0(percent(mu-sigma, accuracy=1), " to ", percent(mu+sigma, accuracy=1))
    rng95 <- paste0(percent(mu-2*sigma, accuracy=1), " to ", percent(mu+2*sigma, accuracy=1))
    div(style="background-color:#f0f0f0; padding:8px; border-radius:5px;",
        p("Expectation path: Deterministic average path using the assumed annual return without random fluctuations."),
        p("Median path: In 50% of scenarios the development is above, in 50% below. More realistic than the expectation path because market fluctuations are included."),
        p("Dark gray area: 50% of all scenarios (25th–75th percentile)."),
        p("Light gray area: 90% of all scenarios (5th–95th percentile)."),
        p(paste0("About 68% of annual returns fall within ",
                 percent(mu,accuracy=1), " ± ", percent(sigma,accuracy=1),
                 " → i.e., between ", rng68, ".")),
        p(paste0("About 95% of annual returns fall within ",
                 percent(mu,accuracy=1), " ± ", percent(2*sigma,accuracy=1),
                 " → i.e., between ", rng95, ".")))
  })
}

shinyApp(ui, server)
