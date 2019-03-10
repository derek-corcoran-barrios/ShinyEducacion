data("mtcars")
library(shiny)
library(tidyverse)
library(broom)
library(miniUI)
Metric <- mtcars
Metric$mpg <- Metric$mpg * 0.425144
Metric$wt <- Metric$wt  * 0.453592
Metric$Prediction <- NA
Metric$Cuadrados <- NA

# Define UI for application that draws a histogram
ui <- miniPage(
    
    # Application title
    gadgetTitleBar("Estimación de parametros basado en mínimos cuadrados"),
    
    
                     miniContentPanel(padding = 0,
                                      fillCol(flex = c(12,1,1,2.5,1,1),plotOutput("distPlot"),
                                              verbatimTextOutput("Cuadrados"),
                                              verbatimTextOutput("Solucion"),
                                              fillRow(sliderInput("pendiente","Estimación de la pendiente:",
                                                                  min = -10,
                                                                  max = 10,
                                                                  value = 0),
                                                      sliderInput("intercepto",
                                                                  "Estimación del intercepto:",
                                                                  min = 5,
                                                                  max = 20,
                                                                  value = 8)),
                                              fillRow(checkboxInput("resid", label = "Mostrar residuales", value = TRUE),
                                                      checkboxInput("checkbox", label = "Mostrar solución", value = FALSE)),
                                              p("Desarrollado por Giorgia Graells y Derek Corcoran")
                                              )
                         
                     )
    
                                              
                        
                     )
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        Metric$Prediction <- input$intercepto + input$pendiente*Metric$wt
        Metric$Cuadrados <- (Metric$mpg - Metric$Prediction)^2
        # draw the histogram with the specified number of bins
        G <- ggplot(Metric, aes(x = wt, y = mpg)) + geom_point() + geom_abline(slope = input$pendiente, intercept = input$intercepto, color = "red", lty = 2) + theme_classic() + xlab("Peso [Toneladas]") + ylab("Rendimiento [Km/L]")
        if (input$checkbox == TRUE){
            G <- G + geom_smooth(method = "lm")  
        }
        if (input$resid == TRUE){
            G <- G + geom_segment(aes(xend = wt, yend = Prediction))
        }
        G
    })
    
    
    output$Cuadrados <- renderPrint({
        Metric$Prediction <- input$intercepto + input$pendiente*Metric$wt
        Metric$Cuadrados <- (Metric$mpg - Metric$Prediction)^2
        SumSquares <- sum(Metric$Cuadrados)
        print(paste("La suma de Cuadrados es", round(SumSquares,3)))
    })
    output$Solucion <- renderText({
        Modelo <- tidy(lm(mpg~wt, data = Metric))
        Metric$Prediction <- Modelo$estimate[1] + Modelo$estimate[2]*Metric$wt
        Metric$Cuadrados <- (Metric$mpg - Metric$Prediction)^2
        SumSquares <- sum(Metric$Cuadrados)
        a <- paste("La pendiente es", round(Modelo$estimate[2], 2), "el intercepto es", round(Modelo$estimate[1], 2), "y la suma de cuadrados es", round(SumSquares, 2))
        if (input$checkbox == TRUE){
            print(a)
        }
    })
    #################Anova
    
    # Define server logic required to draw a histogram
    server <- function(input, output) {
        
        output$distPlot <- renderPlot({
            # generate bins based on input$bins from ui.R
            x    <- faithful[, 2] 
            bins <- seq(min(x), max(x), length.out = input$bins + 1)
            
            # draw the histogram with the specified number of bins
            hist(x, breaks = bins, col = 'darkgray', border = 'white')
        })
    }
    
}
# Run the application 
shinyApp(ui = ui, server = server)

