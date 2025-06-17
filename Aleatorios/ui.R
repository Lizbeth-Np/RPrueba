library(shiny)
library(kableExtra)
library(shinythemes)
library(DT)
library(shinycssloaders)

fluidPage(
  theme = shinytheme("flatly"), 
  
  titlePanel(
    div(
      style = "padding: 20px;",
      tags$h2("🎲 Generación de números aleatorios", style = "color: #1D3557; font-weight: bold;"),
      tags$h4("Método congruencial multiplicativo y mixto", style = "color: #457B9D; font-style: italic;")
    )
  ),
  
  tabsetPanel(
    tabPanel("Números Aleatorios",
             sidebarLayout(
               sidebarPanel(
                 style = "
                   background-color: #ffffff;
                   border-left: 5px solid #1D3557;
                   border-radius: 12px;
                   padding: 25px;
                   box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
                 ",
                 
                 tags$h4(icon("seedling"), span("Valor inicial (semilla x₀):", style = "color:#2C3E50; font-weight: 600;")),
                 sliderInput("semilla", NULL, min = 1, max = 500, value = 30, width = "100%"),
                 
                 tags$h4(span("Valor de m (módulo):", style = "color:#2C3E50; font-weight: 600;")),
                 sliderInput("divisor", NULL, min = 1, max = 500, value = 335, width = "100%"),
                 
                 tags$h4(span("Valor de a (multiplicador):", style = "color:#2C3E50; font-weight: 600;")),
                 sliderInput("constante", NULL, min = 1, max = 500, value = 123, width = "100%"),
                 
                 tags$h4(span("Cantidad de números a generar:", style = "color:#2C3E50; font-weight: 600;")),
                 sliderInput("num", NULL, min = 1, max = 200, value = 50, width = "100%"),
                 
                 tags$h4(span("Valor de c (constante aditiva):", style = "color:#2C3E50; font-weight: 600;")),
                 sliderInput("c", NULL, min = 1, max = 500, value = 7, width = "100%"),
                 
                 br(),
                 actionButton("mostrar", " Mostrar resultados", icon = icon("chart-bar"),
                              style = "background-color: #2C3E50; color: white; width: 100%; height: 45px; font-size: 16px; font-weight: bold; border-radius: 6px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
                 )
               ),
               
               mainPanel(
                 style = "background-color: white; border-radius: 8px; padding: 20px;",
                 
                 conditionalPanel(
                   condition = "input.mostrar!=0",
                   div(
                     style = "background-color: #f8f9fa; border-radius: 6px; padding: 15px; margin-bottom: 20px;",
                     h4("Tabla de resultados - Método Congruencial Multiplicativo:", style = "color: #2c3e50;"),
                     withSpinner(tableOutput("tabla"), color = "#1D3557")
                   ),
                   
                   div(
                     style = "background-color: #f8f9fa; border-radius: 6px; padding: 15px; margin-bottom: 20px;",
                     h4("Tabla de resultados - Método Congruencial Mixto:", style = "color: #2c3e50;"),
                     withSpinner(tableOutput("tabla1"), color = "#1D3557")
                   ),
                   
                   div(
                     style = "background-color: #f8f9fa; border-radius: 6px; padding: 15px;",
                     h4("Distribución de valores:", style = "color: #2c3e50;"),
                     fluidRow(
                       column(width = 3,
                              numericInput("barras", "Número de barras:", value = 10, min = 2, max = 20)
                       ),
                       column(width = 4,
                              div(style = "text-align: center;",
                                  h5("Método Multiplicativo", style = "color: #063970; font-weight: bold;"),
                                  withSpinner(plotOutput("hist_multiplicativo", height = "250px"), color = "#1D3557")
                              )
                       ),
                       column(width = 4,
                              div(style = "text-align: center;",
                                  h5("Método Mixto", style = "color: #063970; font-weight: bold;"),
                                  withSpinner(plotOutput("hist_mixto", height = "250px"), color = "#1D3557")
                              )
                       )
                     )
                   )
                 )
               )
             )
    ),
    
    tabPanel("Integrales",
             sidebarLayout(
               sidebarPanel(
                 style = "background-color: #ffffff; border-left: 5px solid #1D3557; border-radius: 12px; padding: 25px; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08); ",
                 
                 tags$h4(span("Función a integrar", style = "color:#2C3E50; font-weight: 600;")),
                 textInput("funcion", NULL, value = "1 - x", 
                           placeholder = "Ejemplo: exp(-x), 1/(1+x^2)", width = "100%"),
                 
                 tags$h4(span("Límite inferior", style = "color:#2C3E50; font-weight: 600;")),
                 numericInput("lim_inf", NULL, value = 0, min = 0, width = "100%"),
                 
                 tags$h4(span("Límite superior", style = "color:#2C3E50; font-weight: 600;")),
                 numericInput("lim_sup", NULL, value = 1, min = 0, width = "100%"),
                 
                 tags$h4(span("Método de generación", style = "color:#2C3E50; font-weight: 600;")),
                 radioButtons("metodo", NULL,
                              choices = c("Congruencial Multiplicativo", "Congruencial Mixto"),
                              selected = "Congruencial Multiplicativo",
                              inline = FALSE,
                              width = "100%"),
                 
                 br(),
                 actionButton(
                   "calcular", 
                   label = HTML("<b>Calcular Área</b>"),
                   icon = icon("calculator"),
                   style = "background-color: #1D3557; color: white; width: 100%; height: 45px; font-size: 16px; border-radius: 6px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
                 )
               ),
               
               mainPanel(
                 style = "background-color: white; border-radius: 8px; padding: 20px;",
                 
                 conditionalPanel(
                   condition = "input.calcular!=0",
                   div(
                     style = "background-color: #f8f9fa; border-radius: 6px; padding: 15px; margin-bottom: 20px;",
                     h4("Gráfica de la función a integrar:", style = "color: #2c3e50;"),
                     withSpinner(plotOutput("graf_fun01"), color = "#1D3557")
                   ),
                   
                   div(
                     style = "background-color: #f8f9fa; border-radius: 6px; padding: 15px;",
                     h4("Aproximación numérica:", style = "color: #2c3e50;"),
                     withSpinner(plotOutput("graf_aprox01"), color = "#1D3557")
                   )
                 )
               )
             )
    ),
    
    tabPanel("Distribuciones Discretas",
             sidebarLayout(
               sidebarPanel(
                 style = "background-color: #ffffff; border-left: 5px solid #1D3557; border-radius: 12px; padding: 25px; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08); ",
                 
                 tags$h4(icon("chart-bar"), "Distribuciones Discretas"),
                 
                 tabsetPanel(
                   tabPanel("Binomial",
                            numericInput("n_binomial", "Número de ensayos (n):", value = 10, min = 1, step = 1),
                            numericInput("p_binomial", "Probabilidad de éxito (p):", value = 0.5, min = 0, max = 1, step = 0.01),
                            numericInput("simulaciones_binomial", "Cantidad de simulaciones:", value = 1000, min = 1, step = 1),
                            actionButton("simular_binomial", "Simular", style = "background-color: #1D3557; color: white; width: 100%; height: 45px; font-size: 16px; border-radius: 6px;")
                   ),
                   tabPanel("Poisson",
                            numericInput("lambda_poisson", "Parámetro λ:", value = 5, min = 0, step = 0.1),
                            numericInput("simulaciones_poisson", "Cantidad de simulaciones:", value = 1000, min = 1, step = 1),
                            actionButton("simular_poisson", "Simular", style = "background-color: #1D3557; color: white; width: 100%; height: 45px; font-size: 16px; border-radius: 6px;")
                   ),
                   tabPanel("Binomial Negativa",
                            numericInput("r_binomial_neg", "Número de éxitos:", value = 5, min = 1, step = 1),
                            numericInput("p_binomial_neg", "Probabilidad de éxito (p):", value = 0.5, min = 0, max = 1, step = 0.01),
                            numericInput("simulaciones_binomial_neg", "Cantidad de simulaciones:", value = 1000, min = 1, step = 1),
                            actionButton("simular_binomial_neg", "Simular", style = "background-color: #1D3557; color: white; width: 100%; height: 45px; font-size: 16px; border-radius: 6px;")
                   ),
                   tabPanel("Otro",
                            numericInput("size_custom", "Tamaño del vector:", value = 3, min = 1, max = 20, step = 1),
                            uiOutput("custom_inputs"),
                            numericInput("simulaciones_custom", "Cantidad de simulaciones:", value = 1000, min = 1, step = 1),
                            withSpinner(textOutput("prob_validation"), color = "#1D3557"),
                            actionButton("simular_custom", "Simular", style = "background-color: #1D3557; color: white; width: 100%; height: 45px; font-size: 16px; border-radius: 6px;")
                   )
                 )
               ),
               
               mainPanel(
                 style = "background-color: white; border-radius: 8px; padding: 20px;",
                 
                 conditionalPanel(
                   condition = "input.simular_binomial!=0 || input.simular_poisson!=0 || input.simular_binomial_neg!=0 || input.simular_custom!=0",
                   div(
                     style = "background-color: #f8f9fa; border-radius: 6px; padding: 15px; margin-bottom: 20px;",
                     h4("Resultados de Simulación:", style = "color: #2c3e50;"),
                     withSpinner(tableOutput("resultados_simulacion"), color = "#1D3557")
                   ),
                   div(
                     style = "background-color: #f8f9fa; border-radius: 6px; padding: 15px; margin-bottom: 20px;",
                     h4("Histograma de Simulaciones:", style = "color: #2c3e50;"),
                     withSpinner(plotOutput("histograma_simulacion", height = "400px"), color = "#1D3557")
                   ),
                   div(
                     style = "background-color: #f8f9fa; border-radius: 6px; padding: 15px;",
                     h4("Resumen Estadístico:", style = "color: #2c3e50;"),
                     withSpinner(verbatimTextOutput("resumen_estadistico"), color = "#1D3557"),
                     br(),
                     withSpinner(downloadButton("descargar_pdf", "Reporte PDF",
                                                style = "background-color: #1D3557; color: white; width: 20%; height: 45px; font-size: 16px; border-radius: 6px;"), color = "#1D3557")
                   )
                 )
               )
             )
    ),
    
    tabPanel("Distribuciones Continuas",
             sidebarLayout(
               sidebarPanel(
                 style = "background-color: #ffffff; border-left: 5px solid #1D3557; border-radius: 12px; padding: 25px; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);",
                 
                 tags$h4(icon("chart-area"), span("Distribución Exponencial", style = "color:#2C3E50; font-weight: 600;")),
                 
                 numericInput("lambda_exp", "Parámetro λ:", value = 3, min = 0.001, step = 0.1, width = "100%"),
                 numericInput("simulaciones_exp", "Cantidad de simulaciones:", value = 100, min = 1, width = "100%"),
                 
                 br(),
                 actionButton(
                   "simular_exp",
                   "Simular",
                   icon = icon("play"),
                   style = "background-color: #2C3E50; color: white; width: 100%; height: 45px; font-size: 16px; font-weight: bold; border-radius: 6px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
                 )
               ),
               
               mainPanel(
                 style = "background-color: white; border-radius: 8px; padding: 20px;",
                 
                 h4("Resultados de Simulación:", style = "color: #2c3e50;"),
                 tableOutput("resultados_exp"),
                 
                 br(),
                 h4("Histograma de Simulaciones:", style = "color: #2c3e50;"),
                 plotOutput("histograma_exp", height = "300px"),
                 
                 br(),
                 h4("Resumen Estadístico:", style = "color: #2c3e50;"),
                 verbatimTextOutput("resumen_exp"),
                 br(),
                 h4("Resultado del test KS:", style = "color: #2c3e50;"),
                 verbatimTextOutput("ks_resultado"),
                 
                 br(),
                 h4("Gráfico del test KS:", style = "color: #2c3e50;"),
                 plotOutput("graf_ks", height = "300px")
                 
               )
             )
    )
    
    
    
    
    
    
  )
)