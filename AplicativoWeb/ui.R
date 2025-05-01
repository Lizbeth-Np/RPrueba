
library(shiny)
tvar <- datos |> map_chr(class) 
tvar <- data.table(Variable = names(tvar) , Tipo = unname(tvar))
# Define UI for application that draws a histogram
fluidPage(
  tags$style("h1 {color: #9A9A9A; font-size:35px}"),
  tags$style("h2 {color: #A569BD; font-size:25px}"),
  fluidRow(column(width = 3, tags$img(src="logo-epn-vertical.png", width = "60px", height = "60px")),
           column(width = 9, h1("Primer aplicativo Simulación", style = "text-align:center"))),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons("tipo", "Seleccione el tipo de variables a analizar:", 
                   choiceNames = c("Cuantitativas", "Cualitativas"),
                   choiceValues = c("numeric", "character"), selected = "numeric"),
      selectInput("var", "Variable seleccionada",
                  choices = tvar$Variable, selected = tvar$Variable[2]),
      selectInput("color", "Seleccione color de las barras:",
                  choices = c( "Lavanda" = "#AF7AC5",
                               "Azul Claro" = "#5DADE2", 
                               "Rojo Coral" = "#FF5733", 
                               "Verde Pasto" = "#58D68D",
                               "Amarillo Oro" = "#F1C40F",
                               "Rosa Palo" = "#F1948A", 
                               "Celeste claro" = "#85C1E9",
                               "Turquesa" = "#48C9B0"),
                  selected = "white"),
      selectInput("border_color", "Seleccione color del borde:",
                  choices = c(
                              "Verde Oliva" = "#808000",
                              "Azul Profundo" = "#1A5276",
                              "Morado Oscuro" = "#512E5F",
                              "Gris Claro" = "#D5D8DC"),
                  selected = "white" ),
      selectInput("plot_size", "Tamaño del gráfico:",
                  choices = c("Pequeño" = "300px",
                              "Mediano" = "400px",
                              "Grande" = "500px"),
                  selected = "400px")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("Resumen:"),
      verbatimTextOutput("resumen"),
      h2("Gráfico"),
      uiOutput("grafico_ui")
    )
  )
)
