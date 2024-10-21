#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
library(shinyFeedback)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),

    # Application title
    #titlePanel("Calculadora de interés"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        
        column(12, offset = 4,
            useShinyFeedback(),
            titlePanel("Calculadora de interés"),
            numericInput("montoTotal", "Ingresa el monto total:", value = 1000, min = 0),
            textInput("cantidadCuotas", "Ingresa la cantidad de cuotas:", value = "3"),
            numericInput("TEA", "Ingresa la TEA (%) (Tasa efectiva anual)", value = 35, min = 0, max = 100),
            h3("Pago de cuota mensual es:"),
            textOutput("pagoCuotaMensual"),
            h3("Pago mensual sin interés es:"),
            textOutput("pagoCuotaSinInteres"),
            h3("El interés mensual es:"),
            textOutput("interesMensual"),
            h3("Total a pagar es:"),
            textOutput("pagoTotal")
            ),
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  calcular_mes_mas_interes <- function(monto, cuotas, tea) {
    
    monto * tea/(1-(1+tea)^(cuotas * -1))
    
  }
  
  formato_numero_output <- function(numero) {
    formatC(numero, digits = 2, format = "f", drop0trailing = TRUE)
  }

  # calcular_interes(1000, 5, 10)

  
    monto <- reactive({
      
      negativo <- input$montoTotal <= 0
      
      shinyFeedback::feedbackWarning("montoTotal", show = negativo, text = "Se requiere un número positivo de monto", color = "red")
      
      req(!negativo)
      
      input$montoTotal
      })
    
    TEA_mensual <- reactive({
      
      
      negativo <- input$TEA <= 0 | !isTruthy(input$TEA)
      
      shinyFeedback::feedbackWarning("TEA", show = negativo, text = "Se requiere un número positivo de TEA", color = "red")
      
      req(!negativo)
      
      ((1 + input$TEA/100)^(30/360)) - 1
      
      
      })
    
    cuota <- reactive({
      
      entero <- str_detect(input$cantidadCuotas, "[:punct:]")
      mayor_a_cero <- as.numeric(input$cantidadCuotas) > 0
      
      shinyFeedback::feedbackWarning("cantidadCuotas", show = entero, text = "Se requiere un número entero positivo de cuotas", color = "red")
      shinyFeedback::feedbackWarning("cantidadCuotas", show = !mayor_a_cero, text = "Se requiere un número positivo de cuotas", color = "red")
      
      req(mayor_a_cero)
      req(!entero)
      
      as.numeric(input$cantidadCuotas)
      
      })
    
    mes_mas_interes <- reactive({
      calcular_mes_mas_interes(monto(), cuota(), TEA_mensual()) 
    })
    
    output$pagoCuotaMensual <- renderText({
        
      if (TEA_mensual() != 0 & TEA_mensual() != "") {
      
        mes_mas_interes() %>% 
          formato_numero_output()

      } else {
        monto()/cuota() %>% formato_numero_output()
      }
        
    })
    
    output$pagoCuotaSinInteres <- renderText({

      (monto()/cuota()) %>% formato_numero_output()
      })
    
    output$interesMensual <- renderText({

      if (TEA_mensual() == 0) {
        
        "No hay interés"
        
      } else {
        (mes_mas_interes() - (monto()/cuota())) %>% 
          formato_numero_output()
        
      }
        })
    
    output$pagoTotal <- renderText({

        if (TEA_mensual() == 0) {
          monto()
        } else {
        
        (mes_mas_interes() * cuota()) %>% 
            formato_numero_output()
        }
        })
    
    # output$histograma_cuotas <- renderPlot({
    #   
    # 
    #   tibble(cuotas = 1:24)
    #   ggplot()
    #   
    # })
      
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
