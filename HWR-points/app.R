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
    theme = shinytheme(theme = "united"),
    
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
            select(Name, wellbeing) %>%filter(! is.na(Name)) %>%
            arrange(desc(wellbeing)) %>%
            ggplot(aes(x = wellbeing, y = Name, fill = Name)) + geom_col() + 
            theme_classic() +  
            labs(title = "HWR 2020 Goal Leaderboard",
                 subtitle = "Wellbeing Points",
                 x = "Total Points",
                 y = "Team Member")
    } else if(input$plot1 == "conditioning"){
        p %>%
            select(Name, conditioning) %>%
            filter(! is.na(Name)) %>%
            arrange(desc(conditioning)) %>%
            ggplot(aes(x = conditioning, y = Name, fill = Name)) + geom_col() + 
            theme_classic() + 
            labs(title = "HWR 2020 Goal Leaderboard",
                 subtitle = "Conditioning Points",
                 x = "Total Points",
                 y = "Team Member")
    }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
