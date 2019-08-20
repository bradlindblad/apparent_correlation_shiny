#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scales)
library(tidyverse)




# UI ----------------------------------------------------------------------

ui <- fluidPage(

    # Application title
    titlePanel("Apparent Correlation in 2 Orthogonal Vectors"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("trials",
                        "Number of trials:",
                        min = 1,
                        max = 1500,
                        value = 10)
            # 
            # ,sliderInput("variables",
            #             "Number of random variables: ",
            #             min = 2, 
            #             max = 20,
            #             value = 2)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("trialplot"),
           tags$div(),
           tags$h1(textOutput("maxx"))
        )
    )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {

    rv <- reactiveValues()
    rv$data <- NULL
    
    observe({
        if(input$trials){
            
            createVecs <- function(x){
                vec1 <- rnorm(n = 20, mean = 0, sd = 1)
                vec2 <- rnorm(n = 20, mean = 0, sd = 1)
                output <- cor(vec1, vec2)
                output <- abs(output)
                return(output)
            }
            
            n <- input$trials
            
            output <- map_chr(seq_len(n), createVecs) %>% as.numeric()
            
            
            mydata <- tibble(trials = seq(1,n,1),
                             correlation_measures = output)
            
            rv$data <- mydata
        }
    })
    
    output$trialplot <- renderPlot({

        ggplot(rv$data) +
            geom_bar(stat = "identity",aes(trials,correlation_measures, fill = correlation_measures),alpha = 0.8) +
            scale_fill_viridis_c() +
            scale_y_continuous(breaks = seq(0,1,0.1), labels = scales::percent_format(accuracy = 1)) +
            theme_minimal() +
            xlab("Trial Number") +
            ylab("Correlation") +
            ggtitle("Apparent Correlation in Two Orthogonal Vectors")
        
        
    })
    
    output$maxx <- renderText({

        final <- max(rv$data$correlation_measures)
        paste0(as.character(round(final * 100,1)), "% maximum correlation")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
