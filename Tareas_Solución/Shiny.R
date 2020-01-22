##### Andres Felipe Ordoñez Casadiego 2019
##### Curso de programación en R nivel intermedio-avanzado
##### Universidad Industrial de Santander - MTR COLEVOL Santander

# Shiny   -------------------------------------------------

## Ejercicio 1. Crear un app que reciba una cantidad de números simulados bajo una distribución normal y que como output sea una grafica tipo histograma y una tabla que muestre la media, la mediana y la sd de los datos

library(shiny)

ui <- fluidPage(numericInput(inputId = "n", 
                             label = "Tamaño de la muestra", 
                             value = 80
),
plotOutput(outputId = "hist"),
dataTableOutput(outputId = "LaTabla")
)


server <- function(input, output){
  
  sliderValues <- reactive({
    
    data.frame(
      Atributo = c("Media",
                   "Mediana",
                   "SD"),
      Valor = as.character(c(mean(rnorm(input$n)),
                             median(rnorm(input$n)),
                             sd(rnorm(input$n)))),
      stringsAsFactors = FALSE)
    
  })
  
  output$LaTabla <- renderDataTable(sliderValues())
  
  output$hist <- renderPlot({
    hist(rnorm(input$n))
  })
  
}

shinyApp(ui, server)

