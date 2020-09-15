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
             uiOutput("Form"),
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
                          gt_output(outputId = "gt_cass")),
                 tabPanel("Bert",
                          gt_output(outputId = "gt_bert")),
                 tabPanel("Rick",
                          gt_output(outputId = "gt_rick")),
                 tabPanel("Pip",
                          gt_output(outputId = "gt_pip")),
                 tabPanel("Kim",
                          gt_output(outputId = "gt_kim")),
                 tabPanel("Nafi",
                          gt_output(outputId = "gt_nafi")),
                 tabPanel("Elianne",
                          gt_output(outputId = "gt_elianne")),
                 tabPanel("Anjalika",
                          gt_output(outputId = "gt_anjalika")),
                 tabPanel("Sofie",
                          gt_output(outputId = "gt_sofie")),
                 tabPanel("Sephora",
                          gt_output(outputId = "gt_sephora")),
                 tabPanel("Michelle",
                          gt_output(outputId = "gt_michelle")),
                 tabPanel("Alicia",
                          gt_output(outputId = "gt_alicia")),
                 tabPanel("Ari",
                          gt_output(outputId = "gt_ari")),
                 tabPanel("Cesca",
                          gt_output(outputId = "gt_cesca")),
                 tabPanel("Chiara",
                          gt_output(outputId = "gt_chiara")),
                 tabPanel("Erin",
                          gt_output(outputId = "gt_erin")),
                 tabPanel("Gabby",
                          gt_output(outputId = "gt_gabby")),
                 tabPanel("Heidi",
                          gt_output(outputId = "gt_heidi")),
                 tabPanel("Becca",
                          gt_output(outputId = "gt_becca")),
                 tabPanel("Emma",
                          gt_output(outputId = "gt_emma")),
                 tabPanel("Christie",
                          gt_output(outputId = "gt_christie")),
                 tabPanel("Yasmeen",
                          gt_output(outputId = "gt_yasmeen"))))
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
        url <- a("HWR Points", href="https://forms.gle/qF6j5fehoiArYVtq9")
        output$Form <- renderUI({
            tagList("Form link:", url)
        })
    
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
    
    output$gt_bert <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Bert") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Bert Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    
    output$gt_rick <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Rick") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Rick Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_pip <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Pip") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Pip Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_kim <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Kim") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Kim Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_nafi <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Nafi") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Nafi Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_elianne <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Elianne") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Elianne Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_anjalika <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Anjalika") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Anjalika Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_sofie <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Sofie") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Sofie Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_sephora <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Sephora") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Sephora Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_michelle <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Michelle") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Michelle Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_alicia <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Alicia") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Alicia Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_ari <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Ari") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Ari Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_cesca <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Cesca") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Cesca Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_chiara <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Chiara") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Chiara Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_erin <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Erin") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Erin Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_gabby <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Gabby") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Gabby Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_heidi <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Heidi") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Heidi Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_becca <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Becca") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Becca Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_emma <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Emma") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Emma Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_christie <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Christie") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Christie Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    output$gt_yasmeen <- render_gt(
        p %>% select(Name, Timestamp, positives, negatives, focus) %>%
            filter(! is.na(Name)) %>% 
            filter(Name == "Yasmeen") %>%
            select(Timestamp, positives, negatives, focus) %>%
            gt() %>%
            tab_header(title = "Yasmeen Reflections") %>%
            cols_label(positives = "Positives from today? 
                       Did you progress towards your personal goals at all?", 
                       negatives = "Negatives from today?
                       How might you grow from this?", 
                       focus = "Focus for tomorrow/the coming days?")
    )
    
}


# Run the application 
shinyApp(ui = ui, server = server)
