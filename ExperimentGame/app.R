#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(nessy)
library(beepr)
library(tidyverse)
library(ggplot2)
library(ggimage)

# Define UI for application that draws a histogram
ui <-
    
    cartridge(shinyjs::useShinyjs(),
    title = "{Design your own experiment}",
    container_with_title("Budget",textOutput("budget")),
    container_with_title(
        title = "Buttons",
        fluidPage(sidebarPanel(
            container_with_title(title = "Plants",
                                 numericInput(inputId= "Plants", label = "Cuantas plantas totales plantarás ($500 each)", 
                                              value = 2, 
                                              min = 0, 
                                              max = 100, 
                                              step = 1)),
            container_with_title(title = "Chambers",
                                 numericInput(inputId= "Camara", label = "Cuantas camaras de cultivo usarás", 
                                              value = 2, 
                                              min = 0, 
                                              max = 100, 
                                              step = 1),
                                 uiOutput("ChamberControls"),
                                 ### Temp
                                 uiOutput("HideTemp"),
                                 shinyjs::hidden(div(id = "advancedT",
                                                     uiOutput("TempControl"))),
                                 ### Watering
                                 uiOutput("HideWater"),
                                 shinyjs::hidden(div(id = "advancedW",
                                                     uiOutput("WateringControl"))),
                                 ### Humidity
                                 uiOutput("HideHum"),
                                 shinyjs::hidden(div(id = "advancedH",
                                                     uiOutput("HumidityControl")))
                                 )
            
        ), mainPanel(
            plotOutput("PlantGraph")
        ))
        
    
        
    )
)

server <- function(input, output, session) {
    
    ## Show/hide contorls of chambers depending on checkboxGroupInput

    
    NewBudget <- reactive({100000 - (10000*input$Camara + 500*input$Plants)})
    output$budget <- renderPrint({prettyNum(Budget <- NewBudget(), big.mark = ",", big.interval = 7L)})
    
    Plants <- reactive({d <- if(input$Camara == 0){NULL} else {
        Cosos <- list()
        for(i in 1:input$Camara){
            Cosos[[i]] <- expand.grid(x = c(1:ceiling(sqrt(floor(input$Plants/input$Camara)))), y = c(1:ceiling(sqrt(floor(input$Plants/input$Camara)))))
            Cosos[[i]]$image <- c(rep("/home/derek/Pictures/Pirana.png", floor(input$Plants/input$Camara)), rep(NA, ((ceiling(sqrt(floor(input$Plants/input$Camara))))^2)- floor(input$Plants/input$Camara)))
            Cosos[[i]]$Chamber <- i
        }  
        
        Cosos <- bind_rows(Cosos)
    }})
    
    
    output$PlantGraph <- renderPlot({if(input$Camara == 0){NULL} else {
        ggplot(Plants(), aes(x, y)) + geom_image(aes(image=image), size=.2)  + theme_void()+ ylim(c(0,(ceiling(sqrt(floor(input$Plants/input$Camara)))+1))) + xlim(c(0,(ceiling(sqrt(floor(input$Plants/input$Camara)))+1))) + facet_wrap(~Chamber)+ theme(panel.background = element_rect(fill = "grey"))
    }})
    #state <- reactiveValues()
    #observe({
    #    state$x <- NewBudget()
    #    tate$y <- ifelse(state$x > 0, beep(2), beep(8))
    #})
    
    # observeEvent(NewBudget() > 0, {
    #     beepr::beep(2)
    # })
    
    ####################################################
    ###############Controles de camara##################
    ####################################################
    
    output$ChamberControls <- renderUI({if(input$Camara == 0){NULL} else {
        checkboxGroupInput("checkGroup", label = "Select controls in your chambers", 
                           choices = list("Temperature" = 1, "Watering" = 2, "Humidity" = 3))
    }})
    
    ###Temp
    
    shinyjs::onclick("TempAdvanced",
                     shinyjs::toggle(id = "advancedT", anim = TRUE))
    
    output$HideTemp <- renderUI({if(input$Camara == 0 | !(1 %in% input$checkGroup)){NULL} else {
        a(id = "TempAdvanced", "Show/hide Temperature info")
    }})
    
    output$TempControl <- renderUI({if(input$Camara == 0 | !(1 %in% input$checkGroup)){NULL} else {
        lapply(1:input$Camara, function(i) {
            sliderInput(inputId = paste0("TempChamber",i),
                        label = paste("Temperature Chamber", i),
                        value = 10,
                        min = 1,
                        max = 30)
        })
    }})
    
    ###Water
    
    shinyjs::onclick("WaterAdvanced",
                     shinyjs::toggle(id = "advancedW", anim = TRUE))
    
    output$HideWater <- renderUI({if(input$Camara == 0 | !(2 %in% input$checkGroup)){NULL} else {
        a(id = "WaterAdvanced", "Success", "Show/hide Watering info")
    }})
    
    output$WateringControl <- renderUI({if(input$Camara == 0 | !(2 %in% input$checkGroup)){NULL} else {
        lapply(1:input$Camara, function(i) {
            sliderInput(inputId = paste0("WaterChamber",i),
                        label = paste("Watering Chamber", i),
                        value = 10,
                        min = 1,
                        max = 300)
        })
    }})
    
    ### Humidity
    
    shinyjs::onclick("HumidityAdvanced",
                     shinyjs::toggle(id = "advancedH", anim = TRUE))
    
    output$HideHum <- renderUI({if(input$Camara == 0 | !(3 %in% input$checkGroup)){NULL} else {
        a(id = "HumidityAdvanced", "Show/hide Humidity info")
    }})
    
    output$HumidityControl <- renderUI({if(input$Camara == 0 | !(3 %in% input$checkGroup)){NULL} else {
        lapply(1:input$Camara, function(i) {
            sliderInput(inputId = paste0("HumidityChamber",i),
                        label = paste("Humidity Chamber", i),
                        value = 10,
                        min = 1,
                        max = 100)
        })
    }})
}



# Run the application 
shinyApp(ui = ui, server = server)

