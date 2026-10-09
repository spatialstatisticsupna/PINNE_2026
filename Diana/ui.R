# ============================================================
# Dianas comparativas de estimadores multi-distribución
# (Normal, Uniforme, Exponencial, Binomial)
# ============================================================
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Comparación de estimadores: Dianas de sesgo y varianza"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Distribución poblacional"),
      selectInput("dist", "Elegir distribución:",
                  choices = c("Normal" = "norm",
                              "Uniforme" = "unif",
                              "Exponencial" = "exp",
                              "Binomial" = "binom")),
      
      # Parámetros dinámicos según distribución
      uiOutput("dist_params"),
      
      h4("Parámetro objetivo (theta)"),
      textInput("theta", "Expresión de theta", value = "mu"),
      helpText("Puedes usar constantes poblacionales (ej: mu, sigma, a, b, lambda, p, size)."),
      
      h4("Estimadores a comparar"),
      helpText("Variables disponibles: x (muestra), n."),
      textInput("formula1", "Estimador 1 (Izquierda)", value = "mean(x)"),
      textInput("formula2", "Estimador 2 (Derecha)", value = "median(x)"),
      
      h4("Muestreo"),
      sliderInput("n", "Tamaño muestral (n)", min = 2, max = 500, value = 20, step = 1),
      sliderInput("B", "Número de réplicas (B)", min = 10, max = 1000, value = 100, step = 10),
      numericInput("seed", "Semilla", value = 123, min = 1),
      
      h4("Límites de la diana"),
      numericInput("radio_max", "Radio máximo común", value = 1.5, min = 0.01, step = 0.5),
      helpText("Mantiene escalas idénticas y fijas en ambas dianas."),
      
      actionButton("go", "Simular", class = "btn-primary", width = "100%")
    ),
    
    mainPanel(
      width = 9,
      helpText(
        "• Centro rojo (0, 0): Valor verdadero del parámetro theta.",
        tags$br(),
        "• Rombo dorado: Centro empírico de las estimaciones (desplazamiento = Sesgo).",
        tags$br(),
        "• Nube azul: Réplicas muestrales (amplitud = Variabilidad muestral)."
      ),
      
      fluidRow(
        column(6, plotOutput("diana1", height = "440px")),
        column(6, plotOutput("diana2", height = "440px"))
      ),
      
      h4("Sesgo, Varianza y Error Cuadrático Medio"),
      tableOutput("stats")
    )
  )
)

server <- function(input, output, session) {
  
  # Interfaz reactiva para los parámetros de la distribución seleccionada
  output$dist_params <- renderUI({
    switch(input$dist,
           "norm" = tagList(
             sliderInput("mu", "Media (mu)", -10, 10, 0, step = 0.5),
             sliderInput("sigma", "Desv. típica (sigma)", 0.1, 5, 1, step = 0.1)
           ),
           "unif" = tagList(
             sliderInput("unif_a", "Límite inferior (a)", -10, 10, 0, step = 0.5),
             sliderInput("unif_b", "Límite superior (b)", -10, 10, 2, step = 0.5)
           ),
           "exp" = tagList(
             sliderInput("lambda", "Tasa (lambda)", 0.1, 5, 1, step = 0.1)
           ),
           "binom" = tagList(
             numericInput("binom_size", "Número de ensayos (size / m)", value = 10, min = 1, step = 1),
             sliderInput("binom_p", "Probabilidad de éxito (p / pi)", min = 0.01, max = 0.99, value = 0.3, step = 0.05)
           )
    )
  })
  
  # Actualizar expresiones por defecto al cambiar de distribución
  observeEvent(input$dist, {
    switch(input$dist,
           "norm" = {
             updateTextInput(session, "theta", value = "mu")
             updateTextInput(session, "formula1", value = "mean(x)")
             updateTextInput(session, "formula2", value = "median(x)")
           },
           "unif" = {
             updateTextInput(session, "theta", value = "b")
             updateTextInput(session, "formula1", value = "max(x)")
             updateTextInput(session, "formula2", value = "(n + 1)/n * max(x)")
           },
           "exp" = {
             updateTextInput(session, "theta", value = "1 / lambda")
             updateTextInput(session, "formula1", value = "mean(x)")
             updateTextInput(session, "formula2", value = "median(x) / log(2)")
           },
           "binom" = {
             updateTextInput(session, "theta", value = "p")
             updateTextInput(session, "formula1", value = "mean(x) / size")
             updateTextInput(session, "formula2", value = "(sum(x) + 1) / (n * size + 2)")
           }
    )
  })
  
  sim <- eventReactive(input$go, {
    set.seed(input$seed + input$go)
    B <- input$B
    n <- input$n
    dist_sel <- input$dist
    f1_txt <- input$formula1
    f2_txt <- input$formula2
    ttxt <- input$theta
    
    tryCatch({
      malo <- "system|file|unlink|source|library|require|Sys\\.|setwd|shell|download|readline|eval|parse|assign|<<-|::"
      for (tx in c(f1_txt, f2_txt, ttxt)) {
        if (!nzchar(trimws(tx))) stop("Hay una fórmula vacía")
        if (grepl(malo, tx)) stop("Expresión no permitida: ", tx)
      }
      
      # Entorno de parámetros poblacionales
      env_pop <- list()
      if (dist_sel == "norm") {
        req(input$mu, input$sigma)
        env_pop <- list(mu = input$mu, sigma = input$sigma)
        X <- matrix(rnorm(B * n, mean = input$mu, sd = input$sigma), nrow = B)
      } else if (dist_sel == "unif") {
        req(input$unif_a, input$unif_b)
        if (input$unif_a >= input$unif_b) stop("El límite inferior 'a' debe ser menor que 'b'")
        env_pop <- list(a = input$unif_a, b = input$unif_b)
        X <- matrix(runif(B * n, min = input$unif_a, max = input$unif_b), nrow = B)
      } else if (dist_sel == "exp") {
        req(input$lambda)
        env_pop <- list(lambda = input$lambda)
        X <- matrix(rexp(B * n, rate = input$lambda), nrow = B)
      } else if (dist_sel == "binom") {
        req(input$binom_size, input$binom_p)
        env_pop <- list(size = input$binom_size, p = input$binom_p, pi = input$binom_p)
        X <- matrix(rbinom(B * n, size = input$binom_size, prob = input$binom_p), nrow = B)
      }
      
      evalua <- function(tx, x = NULL) {
        env <- env_pop
        if (!is.null(x)) env <- c(env, list(x = x, n = length(x)))
        eval(parse(text = tx), envir = env, enclos = parent.frame())
      }
      
      th <- evalua(ttxt)
      if (length(th) != 1 || !is.finite(th)) stop("theta debe ser un escalar finito único")
      
      est1 <- vapply(seq_len(B), function(i) as.numeric(evalua(f1_txt, X[i, ])), numeric(1))
      est2 <- vapply(seq_len(B), function(i) as.numeric(evalua(f2_txt, X[i, ])), numeric(1))
      
      z_y <- rnorm(B, mean = 0, sd = 1)
      
      list(
        est1 = est1,
        est2 = est2,
        th = th,
        z_y = z_y,
        formula1 = f1_txt,
        formula2 = f2_txt,
        R_max = max(input$radio_max, 0.001)
      )
    }, error = function(e) {
      validate(need(FALSE, paste0("Error en el cálculo: ", conditionMessage(e))))
    })
  }, ignoreNULL = FALSE)
  
  # Dibujar diana de tiro
  dibuja_diana <- function(est, th, z_y, R_max, titulo, sub_txt) {
    x_impactos <- est - th
    s_est <- if (length(est) > 1) sd(est) else 0
    y_impactos <- z_y * s_est
    
    df_pts <- data.frame(x = x_impactos, y = y_impactos)
    
    t <- seq(0, 2 * pi, length.out = 300)
    r_anillos <- seq(R_max / 3, R_max, length.out = 3)
    
    r_centro_gris <- r_anillos[1]
    circ_gris <- data.frame(x = r_centro_gris * cos(t), y = r_centro_gris * sin(t))
    
    df_circulos <- do.call(rbind, lapply(r_anillos, function(r) {
      data.frame(x = r * cos(t), y = r * sin(t), radio = r)
    }))
    
    etiquetas <- data.frame(
      x = r_anillos,
      y = 0,
      label = as.character(round(r_anillos, 3))
    )
    
    x_medio <- mean(x_impactos)
    y_medio <- mean(y_impactos)
    
    ggplot() +
      geom_polygon(data = circ_gris, aes(x, y), fill = "#e0e0e0", alpha = 0.8) +
      geom_path(data = data.frame(x = (r_centro_gris / 2) * cos(t), y = (r_centro_gris / 2) * sin(t)),
                aes(x, y), colour = "grey20", linewidth = 0.7) +
      geom_path(data = df_circulos, aes(x, y, group = radio), colour = "grey55", linewidth = 0.6) +
      geom_hline(yintercept = 0, colour = "grey65", linewidth = 0.5) +
      geom_vline(xintercept = 0, colour = "grey65", linewidth = 0.5) +
      geom_text(data = etiquetas, aes(x = x + (R_max * 0.05), y = -(R_max * 0.06), label = label),
                size = 3.2, colour = "grey25") +
      geom_point(data = df_pts, aes(x, y), colour = "#1f77b4", size = 1.8, alpha = 0.7) +
      annotate("point", x = x_medio, y = y_medio, shape = 23, size = 5.5,
               fill = "gold", colour = "black", stroke = 1.2) +
      annotate("point", x = 0, y = 0, colour = "red", fill = "red", size = 3) +
      coord_fixed(xlim = c(-R_max * 1.1, R_max * 1.1),
                  ylim = c(-R_max * 1.1, R_max * 1.1),
                  expand = FALSE) +
      labs(title = titulo, subtitle = sub_txt) +
      theme_void(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11, colour = "grey30"),
        plot.margin = margin(10, 10, 10, 10)
      )
  }
  
  output$diana1 <- renderPlot({
    d <- sim()
    sesgo1 <- mean(d$est1) - d$th
    v1 <- if (length(d$est1) > 1) var(d$est1) else NA_real_
    dibuja_diana(d$est1, d$th, d$z_y, d$R_max,
                 titulo = paste0("Estimador 1: ", d$formula1),
                 sub_txt = paste0("Sesgo = ", signif(sesgo1, 3), " | Var = ", signif(v1, 3)))
  })
  
  output$diana2 <- renderPlot({
    d <- sim()
    sesgo2 <- mean(d$est2) - d$th
    v2 <- if (length(d$est2) > 1) var(d$est2) else NA_real_
    dibuja_diana(d$est2, d$th, d$z_y, d$R_max,
                 titulo = paste0("Estimador 2: ", d$formula2),
                 sub_txt = paste0("Sesgo = ", signif(sesgo2, 3), " | Var = ", signif(v2, 3)))
  })
  
  output$stats <- renderTable({
    d <- sim()
    R1 <- d$est1 - d$th
    R2 <- d$est2 - d$th
    
    v1 <- if (length(R1) > 1) var(R1) else NA_real_
    v2 <- if (length(R2) > 1) var(R2) else NA_real_
    
    data.frame(
      "Estimador" = c(paste0("1: ", d$formula1), paste0("2: ", d$formula2)),
      "theta real" = c(d$th, d$th),
      "Media estim." = c(mean(d$est1), mean(d$est2)),
      "Sesgo = E(R)" = c(mean(R1), mean(R2)),
      "Varianza = Var(R)" = c(v1, v2),
      "ECM = E(R^2)" = c(mean(R1^2), mean(R2^2)),
      check.names = FALSE
    )
  }, digits = 4)
}

shinyApp(ui, server)
