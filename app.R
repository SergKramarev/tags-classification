library(shiny)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
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
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
    
    # Upload table
    df <- function(){
       
        req(input$file1)
        
        tryCatch(
            {
                df <- read.csv(input$file1$datapath, sep = ";", header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        names(df)[1] <- "no."
        df <- dplyr::select(df, Keyword, Volume)
        df$n_words <- tokenizers::count_words(df$Keyword)
        return(df)
    }
    
    # use reactive Values
    
    # Bigrams
    
    bigrams <- function(){
        up_t <- up_t[up_t$n_words >= 2, ]
        up_t <- head(up_t, 2500)
        bigrams <- unlist(tokenizers::tokenize_ngrams(up_t$Keyword, n = 2, lowercase = TRUE))
        bigrams <- as.data.frame(bigrams) %>%
            dplyr::group_by(bigrams) %>%
            dplyr::summarise(Freq = n()) %>%
            dplyr::arrange(desc(Freq))
        
       bigrams$Sum_Volume <- sapply(unlist(bigrams$bigrams, use.names = FALSE), FUN = function(x){
            sum(up_t[grepl(paste("\\b", x, "\\b", sep =""), up_t$Keyword), "Volume"], na.rm = TRUE) })
        
        bigrams$Mean_of_Volume <- round(bigrams$Sum_Volume/bigrams$Freq)
        
        return(bigrams)
    }
    
    trigrams <- function(){
      up_t <- head(up_t, 2500)
      #up_t <- dplyr::filter(up_t, Volume >= input$volume_limit_t)
      up_t <- up_t[up_t$n_words >= 3, ]
      trigrams <- unlist(tokenizers::tokenize_ngrams(up_t$Keyword, n = 3, lowercase = TRUE))
      trigrams <- as.data.frame(trigrams) %>%
        dplyr::group_by(trigrams) %>%
        dplyr::summarise(Freq = n()) %>%
        dplyr::arrange(desc(Freq))
      
      trigrams$Sum_Volume <- sapply(unlist(trigrams$trigrams, use.names = FALSE), FUN = function(x){
        sum(up_t[grepl(paste("\\b", x, "\\b", sep =""), up_t$Keyword), "Volume"], na.rm = TRUE) })
     
      
      trigrams$Mean_of_Volume <- round(trigrams$Sum_Volume/trigrams$Freq)
      
      return(trigrams)
      
    }
    
    fourgrams <- function() {
      up_t <- head(up_t, 2500)
      #up_t <- dplyr::filter(up_t, Volume >= input$freq_limit_f)
      up_t <- up_t[up_t$n_words >= 4, ]
      fourgrams <- unlist(tokenizers::tokenize_ngrams(up_t$Keyword, n = 4, lowercase = TRUE))
      fourgrams <- as.data.frame(fourgrams) %>%
        dplyr::group_by(fourgrams) %>%
        dplyr::summarise(Freq = n()) %>%
        dplyr::arrange(desc(Freq))
      
      fourgrams$Sum_Volume <- sapply(unlist(fourgrams$fourgrams, use.names = FALSE), FUN = function(x){
        sum(up_t[grepl(paste("\\b", x, "\\b", sep =""), up_t$Keyword), "Volume"], na.rm = TRUE) })
     
      fourgrams$Mean_of_Volume <- round(fourgrams$Sum_Volume/fourgrams$Freq)
      
      return(fourgrams)
    }
    
   
   observeEvent(input$file1, {
       up_t <<- df()
       output$uploaded_table <- DT::renderDataTable({DT::datatable(up_t, selection = list(mode = "single")) })
   })
   
   observeEvent(input$calc_bigrams, {
       bigr <<- bigrams()
       output$bigrams <- DT::renderDataTable({ DT::datatable( bigr, 
                                                              selection = list(mode = "single"),
                                                              extensions = c("Buttons"),
                                                              options = list(
                                                                pageLength = 25,
                                                                dom = "Bfrtip",
                                                                buttons = c("copy")
                                                                )) })
   })
   
   observeEvent(input$bigrams_rows_selected, {
      output$additional_b <- DT::renderDataTable({ DT::datatable( up_t[grepl(paste("\\b", unlist(bigr[input$bigrams_rows_selected, "bigrams"]), "\\b", sep =""), up_t$Keyword), c("Keyword", "Volume")],
                                                                selection = list(mode = "single"),
                                                                extensions = c("Buttons"),
                                                                options = list(
                                                                  pageLength = 100,
                                                                  dom = "Bfrtip",
                                                                  buttons = "copy"
                                                                )) })
   })
   
   observeEvent(input$calc_trigrams, {
     trigr <<- trigrams()
     output$trigrams <- DT::renderDataTable({ DT::datatable(trigr, 
                                                            selection = list(mode = "single"),
                                                            extensions = c("Buttons"),
                                                            options = list(
                                                              pageLength = 25,
                                                              dom = "Bfrtip",
                                                              buttons = c("copy")
                                                            )) })  
     })
   observeEvent( input$trigrams_rows_selected, {
     output$additional_t <- DT::renderDataTable({ DT::datatable( up_t[grepl(paste("\\b", unlist(trigr[input$trigrams_rows_selected, "trigrams"]), "\\b", sep =""), up_t$Keyword), c("Keyword", "Volume")],
                                                                 selection = list(mode = "single"),
                                                                 extensions = c("Buttons"),
                                                                 options = list(
                                                                   pageLength = 100,
                                                                   dom = "Bfrtip",
                                                                   buttons = "copy"
                                                                 )) })
   })

  
    observeEvent(input$calc_fourgrams, {
      four <<- fourgrams()
      output$fourgrams <- DT::renderDataTable({ DT::datatable(four, 
                                                              selection = list(mode = "single"),
                                                              extensions = c("Buttons"),
                                                              options = list(
                                                                pageLength = 25,
                                                                dom = "Bfrtip",
                                                                buttons = c("copy")
                                                              )) })  
    })
    
    observeEvent(input$fourgrams_rows_selected, { 
      output$additional_f <- DT::renderDataTable({ DT::datatable( up_t[grepl(paste("\\b", unlist(four[input$fourgrams_rows_selected, "fourgrams"]), "\\b", sep =""), up_t$Keyword), c("Keyword", "Volume")],
                                                                  selection = list(mode = "single"),
                                                                  extensions = c("Buttons"),
                                                                  options = list(
                                                                    pageLength = 100,
                                                                    dom = "Bfrtip",
                                                                    buttons = "copy"
                                                                  )) })
      } )
   
}

# Run the application 
shinyApp(ui = ui, server = server)
