## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
  navbarPage(
    "Movie Recommender",
    
    tabPanel("By Genres",
             dashboardPage(
               skin = "blue",
               dashboardHeader(disable = TRUE),
               
               dashboardSidebar(disable = TRUE),
               
               dashboardBody(includeCSS("css/movies.css"),
                             
                             box(width = 12, title = "Step 1: Choose a movie genre", status = "info", solidHeader = TRUE, collapsible = FALSE,
                                 tags$style(type='text/css', 
                                            ".selectize-input { font-size: 18px; line-height: 18px;} .selectize-dropdown { font-size: 16px; line-height: 16px; }"),
                                 selectInput("genre", label = NULL,
                                             choices = c("Action", "Adventure", "Animation", "Children.s", "Comedy", "Crime", 
                                                         "Documentary", "Drama", "Fantasy", "Film.Noir", "Horror", "Musical", 
                                                         "Mystery", "Romance", "Sci.Fi", "Thriller", "War", "Western"))
                             ),
                             
                             # actionButton("update", "Update"),
                             # 
                             # h4("Recommendation"),
                             # tableOutput("view")
                             
                             box(
                               width = 12, status = "info", solidHeader = TRUE,
                               title = "Step 2: Discover movies you might like",
                               br(),
                               withBusyIndicatorUI(
                                 actionButton("btn1", "Click here to get your recommendations", class = "btn-warning")
                               ),
                               br(),
                               tableOutput("results1")
                             )
               )
               
             )
    ),    
    
    tabPanel("By Rating",
             dashboardPage(
               skin = "blue",
               dashboardHeader(disable = TRUE),
               
               dashboardSidebar(disable = TRUE),
               
               dashboardBody(includeCSS("css/movies.css"),
                             fluidRow(
                               box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                   div(class = "rateitems",
                                       uiOutput('ratings')
                                   )
                               )
                             ),
                             fluidRow(
                               useShinyjs(),
                               box(
                                 width = 12, status = "info", solidHeader = TRUE,
                                 title = "Step 2: Discover movies you might like",
                                 br(),
                                 withBusyIndicatorUI(
                                   actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                                 ),
                                 br(),
                                 tableOutput("results")
                               )
                             )
               )
             )
    ),
    
    tabPanel("About",
             fluidPage(
                 mainPanel(
                   tags$div(
                     HTML('<span style="font-family: Arial, sans-serif; font-size: 18px; color: gray;">
                          Project Team<br>Member: Chaochao Zhou (cz76@illinois.edu)<br><br>Acknowledgement<br>https://github.com/pspachtholz/BookRecommender</span>'),
                   )
                 )
             )
    )
    
  )  
) 