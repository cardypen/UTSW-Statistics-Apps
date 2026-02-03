# app.R
# Shiny app: Two-sample difference of means with group SDs, Type I/II shading, and power
# H0: Delta = 0 (right-tailed test by default)
# Author: You + M365 Copilot

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Two-Sample Power: Group SDs, Type I (α), Type II (β)"),
  sidebarLayout(
    sidebarPanel(
      h4("Means"),
      numericInput("mu0", "Group 0 mean (μ₀)", value = 0, step = 0.1),
      numericInput("delta", "Mean difference Δ = μ₁ − μ₀", value = 2, step = 0.1),
      checkboxInput("lockDelta", "Lock mean difference (μ₁ = μ₀ + Δ)", TRUE),
      numericInput("mu1", "Group 1 mean (μ₁)", value = 2, step = 0.1),
      
      hr(),
      h4("Group Standard Deviations"),
      sliderInput("s0", "Group 0 SD (s₀)", min = 0.2, max = 20, value = 6, step = 0.1),
      sliderInput("s1", "Group 1 SD (s₁)", min = 0.2, max = 20, value = 6, step = 0.1),
      
      hr(),
      h4("Sample Sizes"),
      sliderInput("n", "Per‑group sample size (n₀ = n₁ = n)", min = 2, max = 1000, value = 25, step = 1),
      actionButton("tighten", "Tighten both (increase n × 1.5)"),
      
      hr(),
      h4("Testing"),
      sliderInput("alpha", "Significance level α (one‑sided, right tail)", 
                  min = 0.001, max = 0.2, value = 0.05, step = 0.001),
      checkboxInput("showAreas", "Shade Type I (α) and Type II (β) areas", TRUE)
    ),
    mainPanel(
      plotOutput("distPlot", height = "480px"),
      br(),
      uiOutput("stats")
    )
  )
)

server <- function(input, output, session) {
  
  # Keep μ1 = μ0 + Δ whenever locked
  observeEvent({ input$mu0; input$delta; input$lockDelta }, {
    if (isTRUE(input$lockDelta)) {
      new_mu1 <- input$mu0 + input$delta
      if (!isTRUE(all.equal(input$mu1, new_mu1))) {
        updateNumericInput(session, "mu1", value = new_mu1)
      }
    }
  }, ignoreInit = TRUE)
  
  # If unlocked, let changing μ1 update Δ
  observeEvent(input$mu1, {
    if (!isTRUE(input$lockDelta)) {
      updateNumericInput(session, "delta", value = input$mu1 - input$mu0)
    }
  })
  
  # Tighten: increase n by 50% (capped at slider max)
  observeEvent(input$tighten, {
    updateSliderInput(session, "n", value = min(round(input$n * 1.5), 1000))
  })
  
  # Parameters and derived quantities for difference in means
  params <- reactive({
    mu0  <- input$mu0
    mu1  <- input$mu1
    dlt  <- mu1 - mu0
    n    <- input$n
    s0   <- input$s0
    s1   <- input$s1
    
    # Sampling SD of the estimator (Xbar1 - Xbar0)
    seD  <- sqrt(s0^2 / n + s1^2 / n)
    
    list(mu0 = mu0, mu1 = mu1, delta = dlt, s0 = s0, s1 = s1, n = n,
         seD = seD, alpha = input$alpha)
  })
  
  # Critical value for right-tailed test on difference: P_H0(D >= c) = alpha
  crit <- reactive({
    p <- params()
    qnorm(1 - p$alpha, mean = 0, sd = p$seD)   # H0 mean is 0 for the difference
  })
  
  # Type II error β and power (under H1 mean = Delta)
  beta <- reactive({
    p <- params()
    pnorm(crit(), mean = p$delta, sd = p$seD)
  })
  power <- reactive({ 1 - beta() })
  
  output$distPlot <- renderPlot({
    p <- params()
    cval <- crit()
    
    # Range covering both H0 (mean 0) and H1 (mean Delta) distributions
    m0 <- 0
    m1 <- p$delta
    se <- p$seD
    
    xmin <- min(m0 - 5 * se, m1 - 5 * se)
    xmax <- max(m0 + 5 * se, m1 + 5 * se)
    xs <- seq(xmin, xmax, length.out = 2000)
    
    df <- data.frame(
      x  = xs,
      d0 = dnorm(xs, mean = m0, sd = se),
      d1 = dnorm(xs, mean = m1, sd = se)
    )
    ymax <- max(df$d0, df$d1)
    
    g <- ggplot(df, aes(x)) +
      geom_line(aes(y = d0), linewidth = 1.2, color = "#222222") +
      geom_line(aes(y = d1), linewidth = 1.2, color = "#222222") +
      theme_minimal(base_size = 14) +
      labs(
        x = NULL, y = "Density of difference in sample means",
        title = "Sampling Distributions of ( X̄₁ − X̄₀ )",
        subtitle = sprintf("μ₀=%.2f, μ₁=%.2f (Δ=%.2f), s₀=%.2f, s₁=%.2f, n=%d, α=%.3f, power=%.3f",
                           p$mu0, p$mu1, p$delta, p$s0, p$s1, p$n, p$alpha, power())
      ) +
      theme(panel.grid.minor = element_blank())
    
    if (isTRUE(input$showAreas)) {
      # Shade Type I (alpha) under H0 to the right of c
      g <- g + geom_area(
        data = subset(df, x >= cval),
        aes(y = d0), fill = "#ff6b6b", alpha = 0.6
      )
      # Shade Type II (beta) under H1 to the left of c
      g <- g + geom_area(
        data = subset(df, x <= cval),
        aes(y = d1), fill = "#06b6d4", alpha = 0.6
      )
    }
    
    g +
      geom_vline(xintercept = cval, color = "gray30", linewidth = 0.8) +
      annotate("text", x = cval, y = ymax * 0.98, label = "Decision threshold", angle = 90,
               vjust = -0.5, size = 4, color = "gray30") +
      annotate("text", x = m0, y = dnorm(m0, m0, se) + ymax * 0.06,
               label = "Null (H₀: Δ=0)", fontface = "bold", size = 4) +
      annotate("text", x = m1, y = dnorm(m1, m1, se) + ymax * 0.06,
               label = "Alternative (H₁: Δ>0)", fontface = "bold", size = 4) +
      annotate("text", x = (cval + xmax) / 2, y = ymax * 0.18,
               label = "Type I error (α)", color = "#ff6b6b", fontface = "bold") +
      annotate("text", x = (xmin + cval) / 2, y = ymax * 0.18,
               label = "Type II error (β)", color = "#06b6d4", fontface = "bold")
  })
  
  output$stats <- renderUI({
    HTML(sprintf(
      "<b>SE(Δ̂):</b> %.4f &nbsp;&nbsp; <b>β (Type II):</b> %.3f &nbsp;&nbsp; <b>Power:</b> %.3f &nbsp;&nbsp; <b>Critical c:</b> %.3f",
      params()$seD, beta(), power(), crit()
    ))
  })
}

shinyApp(ui, server)