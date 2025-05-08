library(shiny)

# Define UI for application that genera números aleatorios
fluidPage(
  
  # Application title
  titlePanel("Generación de números aleatorios - Método congruencial"),
  
  # Tabset para diferentes secciones
  tabsetPanel(
    
    # Primera pestaña: generación de números
    tabPanel("Numeros aleatorios",
             sidebarLayout(
               sidebarPanel(
                 numericInput("Semilla",
                              "Ingrese un valor inicial:",
                              min = 1,
                              max = 50,
                              value = 30),
                 numericInput("divisor",
                              "Ingrese un valor de m:",
                              min = 1,
                              max = 50,
                              value = 30),
                 numericInput("Constante",
                              "Ingrese un valor de a:",
                              min = 1,
                              max = 50,
                              value = 30),
                 numericInput("num",
                              "Cantidad de números a generar:",
                              min = 1,
                              max = 50,
                              value = 30)
               ),
               mainPanel(
                 h4("Tabla de resultados- -Método Congruencial"),
                 br(),
                 tableOutput("tabla"),
                 br(),
                 h4("Tabla de resultados -Método Congruencial MIxto "),
                 br(),
                 br(), #aqui va la segunda tabla
                 br(),
                 h2("Gráfico:"),
                 fluidRow(
                   column(2,
                          numericInput("barras", "Número de barras:", value = 10,min = 2,max = 20),
                          ),
                   column(5,
                          h5("Método 1")),
                   column(5,
                          h5("Método 2"))
                 )
                 
               )
             )
    ),
    
    # Segunda pestaña: Resultados
    tabPanel("Resultados",
             h1("Aquí se muestran los resultados")
    ),
    
    # Tercera pestaña: Integrales
    tabPanel("Integrales",
             h1("Estimación de integrales")
    )
    
  ) # fin tabsetPanel
)
