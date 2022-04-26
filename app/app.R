# libraries ----
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(sf)
  library(tidytext)
  library(topicmodels)
  library(reshape2)
})

# setup ----


# functions ----
source("scripts/func.R") # helper functions

day6_data <- day6_data()
dtm <- get_dtm()

# user interface ----

## tabs ----

### intro_tab ----
intro_tab <- tabItem(
  tabName = "intro_tab",
  p("For Day 26: Interactive, I'm going to make interactive versions of a few of the charts I made for previous days of the 30DCC."),
  img(src = "img/30DCC.png", width = 500, height = 500)
)

### day6_tab ----
day6 <- tabItem(
    tabName = "day6",
    h2("Share of the population using the internet"),
    sliderInput("day6_year", 
                label = "Year", 
                min = 2009, 
                max = 2019, 
                value = 2019,
                step = 1, 
                sep = ""),
    plotOutput("day6_plot"),
    HTML("<caption>Recreation of <a href='https://ourworldindata.org/technology-adoption#share-of-population-with-internet-access'>OurWorldInData</a> | Plot by <a href='https://twitter.com/lisadebruine'>@lisadebruine</a></caption>")
)

### day18_tab ----
day18 <- tabItem(
  tabName = "day18",
  h2("LDA Topic Analysis of OECD Data Insights"),
  fluidRow(
    box(
      numericInput("day18_k", 
                   label = "Number of Topics",
                   min = 2, max = 16, value = 6, step = 1),
      numericInput("day18_topterms", 
                   label = "Number of Terms per Topic",
                   min = 1, max = 20, value = 7, step = 1),
      downloadButton("day18_dl", "Download Chart")
    ),
    box(
      textAreaInput("day18_topics", 
                    label = "Topic Labels (one per line)",
                    value = "global covid effects\nhousehold economics\neconomic recovery\npandemic recovery\neconomic growth\npersonal covid effects",
                    rows = 6),
      actionButton("day18_update_topics", "Update Topic Labels")
    )
  ),
  plotOutput("day18_plot", width = 800, height = 500),
  HTML("<caption>Data from <a href='https://www.oecd.org/coronavirus/en/data-insights/'>OECD</a> | Plot by <a href='https://twitter.com/lisadebruine'>@lisadebruine</a></caption>")
)

## UI ----
ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "30-Day Chart Challenge 2022", 
        titleWidth = "calc(100% - 44px)" # puts sidebar toggle on right
    ),
    dashboardSidebar(
        # https://fontawesome.com/icons?d=gallery&m=free
        sidebarMenu(
            id = "tabs",
            menuItem("Intro", tabName = "intro_tab", icon = icon("calendar")),
            menuItem("Day 6: OWiD", tabName = "day6", icon = icon("globe")),
            menuItem("Day 18: OECD", tabName = "day18", icon = icon("globe"))
        )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
            # links to files in www/
            tags$link(rel = "stylesheet", type = "text/css", href = "basic_template.css"), 
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), 
            tags$script(src = "custom.js")
        ),
        tabItems(
          intro_tab, day6, day18
        )
    )
)


# server ----
server <- function(input, output, session) {
  # day6_plot ----
    output$day6_plot <- renderPlot({ debug_msg("day6_plot")
      day6_plot(day6_data, input$day6_year)
    })
    
    # topic_labels ----
    topic_labels <- reactiveVal(c("global covid effects",
                                     "household economics",
                                     "economic recovery",
                                     "pandemic recovery",
                                     "economic growth",
                                     "personal covid effects"))
      
    # day18_update_topics ----
    observeEvent(input$day18_update_topics, { debug_msg("day18_update_topics")
      paste0(input$day18_topics, " ") %>%
        strsplit(split = "\n") %>%
        `[[`(1) %>% 
        trimws() %>%
        rep(length.out = input$day18_k) %>%
        topic_labels()
    })
    
    # lda ----
    lda <- reactive({ debug_msg("lda")
      tryCatch( 
        get_lda(dtm, k = input$day18_k),
        error = function(e) debug_msg(e$message) 
      )
    })
    
    # tt ----
    tt <- reactive({ debug_msg("day18_topterms")
      tryCatch( 
        get_top_terms(lda(), input$day18_topterms),
        error = function(e) debug_msg(e$message) 
      )
    })
    
    # plot18 ----
    plot18 <- reactive({ debug_msg("plot18")
      tryCatch( 
        day18_plot(req(tt()), topic_labels()),
        error = function(e) debug_msg(e$message) 
      )
    })
    
    # day18_plot ---- 
    output$day18_plot <- renderPlot({ debug_msg("day18_plot")
      plot18() 
    })
    
    # day18_dl ----
    output$day18_dl <- downloadHandler(
      filename = function() {
        "OECD_topic_model.png"
      },
      content = function(file) {
        debug_msg("day18_dl")
        ggsave(file, plot18(), width = 8, height = 5, dpi = 100)
      }
    )
} 

shinyApp(ui, server)