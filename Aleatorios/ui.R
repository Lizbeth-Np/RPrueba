#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Generación de números aleatorios-Método congruencial"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("Semilla",
                        "Ingrese un valor incial:",
                        min = 1,
                        max = 50,
                        value = 30),
            sliderInput("divisor",
                        "Ingrese un valor  de m:",
                        min = 1,
                        max = 50,
                        value = 30),
            sliderInput("Constante",
                        "Ingrese un valor de a:",
                        min = 1,
                        max = 50,
                        value = 30),
            sliderInput("num",
                        "Cantidad de numeros a generar:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("tabla")
        )
    )
)


