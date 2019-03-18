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

# Define UI for application that draws a histogram
ui <-
    
    cartridge(
    title = "{Design your own experiment}",
    container_with_title("Budget",textOutput("budget")),
    container_with_title(
        title = "Buttons",
        radio_buttons("sure", "Are you sure?", c("yes", "no")),
        fluidPage(sidebarPanel(
            numericInput(inputId= "Camara", label = "Cuantas camaras de cultivo usarás", 
                         value = 2, 
                         min = 0, 
                         max = 100, 
                         step = 1),
            numericInput(inputId= "Plants", label = "Cuantas plantas totales plantarás", 
                         value = 2, 
                         min = 0, 
                         max = 100, 
                         step = 1),
            uiOutput("moreControls")
        ), mainPanel(
            plotOutput("PlantGraph")
        ))
        
    
        
    )
)

server <- function(input, output, session) {
    
    NewBudget <- reactive({100000 - 10000*input$Camara})
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
    output$moreControls <- renderUI({if(input$Camara == 0){NULL} else {
        lapply(1:input$Camara, function(i) {
            sliderInput(inputId = paste0("TempChamber",i),
                        label = paste("Temperature Chamber", i),
                        value = 10,
                        min = 1,
                        max = 20)
        })
    }})
}



# Run the application 
shinyApp(ui = ui, server = server)

