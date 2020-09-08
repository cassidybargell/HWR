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
library(gt)

gs4_deauth()
p <- read_sheet("https://docs.google.com/spreadsheets/d/10duGZfrecgT0eqAuymkwYiXSiLNBLLUAiiQQhriM9TU/edit?usp=sharing")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Harvard Women's Rugby Point System",
    theme = shinytheme(theme = "united"),
    
    tabPanel("Point Tracker",
             imageOutput("team", width = "100%", height = "100%"),
             br(),
             tabsetPanel(
                 tabPanel("HWR Fall 2020 Points System",
                          sidebarPanel(
                              helpText("Select to compare between:"),
                              span(),
                              selectInput("plot1", "Categories:",
                                          choices = list("Wellbeing" = "wellbeing",
                                                         "Physical" = "physical",
                                                         "Tactical" = "tactical",
                                                         "Technical" = "technical",
                                                         "Mental" = "mental"),
                                          selected = "wellbeing")),
                          mainPanel(plotOutput("points_plot"))))),
    
    tabPanel("Reflection Entries",
             tabsetPanel(
                 tabPanel("Ach",
                          gt_output(outputId = "gt_ach")),
                 tabPanel("Cass",
                          gt_output(outputId = "gt_cass"))))
        )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$team <- renderImage({
        
        list(src = 'images/team.jpeg',
             height = 450,
             width = 700,
             style = "display: block; margin-left: auto; margin-right: auto;")},
        deleteFile = FALSE
    )
    
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
    } else if(input$plot1 == "physical"){
        p %>%
            select(Name, physical) %>%
            filter(! is.na(Name)) %>%
            arrange(desc(physical)) %>%
            ggplot(aes(x = physical, y = Name, fill = Name)) + geom_col() + 
            theme_classic() + 
            labs(title = "HWR 2020 Goal Leaderboard",
                 subtitle = "Physical Points",
                 x = "Total Points",
                 y = "Team Member")
    } else if(input$plot1 == "tactical"){
        p %>%
            select(Name, tactical) %>%
            filter(! is.na(Name)) %>%
            arrange(desc(tactical)) %>%
            ggplot(aes(x = tactical, y = Name, fill = Name)) + geom_col() + 
            theme_classic() + 
            labs(title = "HWR 2020 Goal Leaderboard",
                 subtitle = "Tactical Points",
                 x = "Total Points",
                 y = "Team Member")
    } else if(input$plot1 == "technical"){
        p %>%
            select(Name, technical) %>%
            filter(! is.na(Name)) %>%
            arrange(desc(technical)) %>%
            ggplot(aes(x = technical, y = Name, fill = Name)) + geom_col() + 
            theme_classic() + 
            labs(title = "HWR 2020 Goal Leaderboard",
                 subtitle = "Technical Points",
                 x = "Total Points",
                 y = "Team Member")
    } else if(input$plot1 == "mental"){
        p %>%
            select(Name, mental) %>%
            filter(! is.na(Name)) %>%
            arrange(desc(mental)) %>%
            ggplot(aes(x = mental, y = Name, fill = Name)) + geom_col() + 
            theme_classic() + 
            labs(title = "HWR 2020 Goal Leaderboard",
                 subtitle = "Mental Points",
                 x = "Total Points",
                 y = "Team Member")
    }
    })
    
    output$gt_ach <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Ach") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Ach Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    
    output$gt_cass <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Cass") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Cass Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    
}


# Run the application 
shinyApp(ui = ui, server = server)
