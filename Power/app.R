library(shiny)
library(tidyverse)
library(broom)
library(miniUI)

# Define UI for application that draws a histogram
ui <- miniPage(
    

    miniTitleBar("Análisis de poder"),
    
    miniContentPanel(
        plotOutput("PowerPlot"),
                  verbatimTextOutput("PowerPrint"),
        sliderInput("K",
                    "Número de grupos a testear:",
                    min = 3, max = 10, value = 5),
        sliderInput("alpha",
                    "Alpha:",
                    min = 0.01, max = 0.99, value = 0.05)
        ,
        sliderInput("poder",
                    "Poder deseado:",
                    min = 0.01, max = 0.99, value = 0.9),
        numericInput("SD", 
                     "Desviación estandar de la muestra:", 0.4, min = 0, max = 1000000)
        ,
        numericInput("Efecto", 
                     "Diferencia mínima a detectar:", 0.5, min = 0, max = 1000000)
        )
)
server <- function(input, output) {
output$PowerPlot <- renderPlot({
    x <- 0
    n <- 2
    power <- list()
    while(x < input$poder) {
        power[[n]] <- broom::tidy(pwr.1way(k=input$K, n=n, alpha=input$alpha, delta=input$Efecto, sigma=input$SD))
        x <- broom::tidy(pwr.1way(k=input$K, n=n, alpha=input$alpha, delta=input$Efecto, sigma=input$SD))$power
        n <- n+1
        if (n == 300){
            break
        }
    }
    
    power <- do.call(rbind, power)
    
    ggplot(power, aes(x = n, y = power)) + geom_line() + ylim(0,1) + theme_classic() + ylab("Poder")
    
})

output$PowerPrint <- renderPrint({
    x <- 0
    n <- 2
    power <- list()
    while(x < input$poder) {
        power[[n]] <- broom::tidy(pwr.1way(k=input$K, n=n, alpha=input$alpha, delta=input$Efecto, sigma=input$SD))
        x <- broom::tidy(pwr.1way(k=input$K, n=n, alpha=input$alpha, delta=input$Efecto, sigma=input$SD))$power
        n <- n+1
        message(n)
        if (n == 300){
            break
        }
    }
    
    power <- do.call(rbind, power)
    paste("La muestra necesaria para el estudio es: ", power$n[nrow(power)], "y el poder es", round(power$power[nrow(power)],4))
})
}

# Run the application 
shinyApp(ui = ui, server = server)
