library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    navbarPage(
        title = "Tags classification",
        tabPanel("Data",
                 sidebarLayout(
                     sidebarPanel(
                         fileInput("file1", "Choose CSV File",
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                         hr(),
                         h4(strong("Bigrams")),
                         actionButton("calc_bigrams", "Посчитать"),
                         br(),
                         hr(),
                         h4(strong("Trigrams")),
                         actionButton("calc_trigrams", "Посчитать"),
                         br(),
                         hr(),
                         h4(strong("Fourgrams")),
                         actionButton("calc_fourgrams", "Посчитать")
                     ),
                     mainPanel(DT::dataTableOutput("uploaded_table"))
                 )),
        tabPanel("Bigrams", column(6, h3("Список биграмов, присутствующих в кейвордах"), br() , DT::dataTableOutput("bigrams")), 
                 column(6, h3("Список кейвордов в которых присутствует выбранный биграм"), br(), DT::dataTableOutput("additional_b"))),
        tabPanel("Trigrams", column(6, h3("Список триграмов, присутствующих в кейвордах"), br() , DT::dataTableOutput("trigrams")), 
                 column(6, h3("Список кейвордов в которых присутствует выбранный триграм"), br(), DT::dataTableOutput("additional_t"))),
        tabPanel("Fourgrams", column(6, h3("Список 4-грамов, присутствующих в кейвордах"), br() , DT::dataTableOutput("fourgrams")), 
                 column(6, h3("Список кейвордов в которых присутствует выбранный 4-грам"), br(), DT::dataTableOutput("additional_f")))
    )
   
))
