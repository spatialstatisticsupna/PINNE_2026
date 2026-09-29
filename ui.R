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