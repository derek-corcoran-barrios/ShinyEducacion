#library(shiny)
library(tidyverse)
library(broom)
library(miniUI)

# Define UI for application that draws a histogram
ui <- miniPage(
    
    # Application title
    miniPage(miniTitleBar("Simulating ANOVA")),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(column(12,plotOutput("ANOVAout"),
                    radioButtons("radio", label = "Tipo de error",
                                 choices = list("Totales" = 1, "Variable" = 2, "Error" = 3),selected = 1, inline=T),
                    sliderInput("muestra",
                                "Número de muestras:",
                                min = 20, max = 80, value = 30),
                    column(6, sliderInput("MediaA", "Media del grupo A:"
                                          , min = 20, max = 80, value = 50),
                           sliderInput("MediaB", "Media del grupo B:"
                                       , min = 20, max = 80, value = 50),
                           sliderInput("MediaC", "Media del grupo C:"
                                       , min = 20, max = 80, value = 50),
                           tableOutput("ANOVAtable")
                           ),
                    column(6, selectInput("DistA", "Distribución Grupo A:",
                                        c("Normal" = "norm",
                                          "Lognormal" = "logn",
                                          "Uniforme" = "unif")),
                           selectInput("DistB", "Distribución Grupo B:",
                                       c("Normal" = "norm",
                                         "Lognormal" = "logn",
                                         "Uniforme" = "unif")),
                           selectInput("DistC", "Distribución Grupo C:",
                                       c("Normal" = "norm",
                                         "Lognormal" = "logn",
                                         "Uniforme" = "unif")),
                           tableOutput("Test"))) 
                             
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    IrisMuestra <- reactive({
        data.frame(Grupo = rep(c("A", "B", "C"), each = input$muestra), Y = c(c(if(input$DistA == "norm"){rnorm(input$muestra, mean = input$MediaA, sd = 5)} else if(input$DistA == "logn"){rlnorm(input$muestra, meanlog = input$MediaA, sdlog = 5)}else if(input$DistA == "unif"){runif(input$muestra, min = input$MediaA/2, max = input$MediaA*2)}),c(if(input$DistB == "norm"){rnorm(input$muestra, mean = input$MediaB, sd = 5)} else if(input$DistB == "logn"){rlnorm(input$muestra, meanlog = input$MediaB, sdlog = 5)}else if(input$DistB == "unif"){runif(input$muestra, min = input$MediaB/2, max = input$MediaB*2)}),c(if(input$DistC == "norm"){rnorm(input$muestra, mean = input$MediaC, sd = 5)} else if(input$DistC == "logn"){rlnorm(input$muestra, meanlog = input$MediaC, sdlog = 5)}else if(input$DistC == "unif"){runif(input$muestra, min = input$MediaC/2, max = input$MediaC*2)}))) %>% group_by(Grupo) %>% mutate(Media.Grupo = mean(Y))
    })
    output$ANOVAout <- renderPlot({
        G <- ggplot(IrisMuestra(), aes(x = Grupo, y = Y)) + geom_boxplot() +  geom_point(position = position_jitter(height = 0L, seed = 1L), aes(color = Grupo)) + theme_classic()
        if (input$radio == 1){
            G <- G + geom_hline(aes(yintercept=mean(Y))) + geom_linerange(aes(x=Grupo, ymax=Y, ymin=mean(Y)),position = position_jitter(height = 0L, seed = 1L))
        }
        
        if (input$radio == 3){
            G <- G  + stat_summary(fun.y=mean, aes(ymin=..y.., ymax=..y.., color = Grupo), geom='errorbar', lwd = 3) + geom_linerange(aes(x=Grupo, ymax=Y, ymin=Media.Grupo),position = position_jitter(height = 0L, seed = 1L))
        }
        
        if (input$radio == 2){
            G <- G  + stat_summary(fun.y=mean, aes(ymin=..y.., ymax=..y.., color = Grupo), geom='errorbar', lwd = 3) + geom_hline(aes(yintercept=mean(Y)))+ geom_linerange(aes(x=Grupo, ymax=mean(Y), ymin=Media.Grupo),position = position_jitter(height = 0L, seed = 1L))
        }
        
        G
    })
    output$ANOVAtable <- renderTable({
        IrisMuestra <- IrisMuestra()
        IrisMuestra$Cuadrados <- (IrisMuestra$Y - mean(IrisMuestra$Y))^2
        IrisMuestra$Residuales <- (IrisMuestra$Y - IrisMuestra$Media.Grupo)^2 
        IrisVariable <- IrisMuestra %>%  group_by(Grupo) %>% summarise(Media = mean(Y), n = n()) %>% mutate(cuadrados = n*(Media -  mean(IrisMuestra$Y))^2)
        TablaANOVA <- data.frame(CuadradosTotales = sum(IrisMuestra$Cuadrados), CudradosVariable = sum(IrisVariable$cuadrados), CuadradosResiduales = sum(IrisMuestra$Residuales))
    })
    output$Test <- renderTable({
        broom::tidy(aov(Y ~ Grupo, data = IrisMuestra()))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

