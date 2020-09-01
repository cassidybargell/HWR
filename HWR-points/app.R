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
library(googlesheets4)
library(shinythemes)
library(ggthemes)

p <- read_sheet("https://docs.google.com/spreadsheets/d/10duGZfrecgT0eqAuymkwYiXSiLNBLLUAiiQQhriM9TU/edit#gid=549236542")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Harvard Women's Rugby Point System",
    theme = shinytheme(theme = "superhero"),
    
    ### GOOGLE FORM

    tabPanel("Leaderboard",
             tabsetPanel(
                 tabPanel("HWR Fall 202 Leaderboard",
                          sidebarPanel(
                              helpText("Select to compare between:"),
                              span(),
                              selectInput("plot1", "Categories:",
                                          choices = list("Wellbeing" = "wellbeing",
                                                         "Conditioning" = "conditioning"),
                                          selected = "wellbeing")),
                          mainPanel(plotOutput("points_plot")))))
        )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$points_plot <- renderPlot({if(input$plot1 == "wellbeing"){
        p %>%
            select(Name, wellbeing) %>%
            ggplot(aes(x = wellbeing, y = Name, fill = Name)) + geom_col() + 
            theme_classic()
    } else if(input$plot1 == "conditioning"){
        p %>%
            select(Name, conditioning) %>%
            ggplot(aes(x = conditioning, y = Name, fill = Name)) + geom_col() + 
            theme_classic()
    }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
