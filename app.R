library(shiny)
library(shinycssloaders)
library(wordcloud)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(tidytext)
library(DT)

source("scrapper/review.R")
source("scrapper/amusement_parks.R")
source("classifier/naive_bayes.R")

features <- readRDS(features_rds_path)

ui <- dashboardPage(
  
  dashboardHeader(title =
                    tags$a(tags$img(src="https://static.tacdn.com/img2/brand_refresh/Tripadvisor_lockup_horizontal_secondary_registered.svg",height = "30px"),'Tripadvisor'
                    )),
  
  dashboardSidebar(
    selectInput(
      "url",
      "Top 25 Amusement Parks - Asia",  
      choices = get_amusement_parks()
    ),
    sliderInput(
      "size",
      "Total reviews",
      min = 0,
      max = 10000,
      value = 10
    ),
    fluidPage(
      submitButton("Submit"),
    )
  ),
  
  dashboardBody(
    # color background
    tags$head(tags$style(HTML('
    .skin-blue .main-header .logo {
    background-color: #e75874;
    }
    .skin-blue .main-header .logo:hover {
    background-color: #ff6684;
    }
    .skin-blue .main-header .navbar {
    background-color: #ff6684;
    }
    '))),
    tags$head(tags$style(HTML('
    .skin-blue .main-sidebar {
      background-color: #be1558;
    }
    .skin-blue .main-header .navbar .sidebar-toggle:hover{
    background-color: #e75874;
    }
    '))),
    fluidRow(
      valueBoxOutput("total_review"),
      valueBoxOutput("positive_review"),
      valueBoxOutput("negative_review")
    ),
    # plot sentiment positive vs negative
    fluidRow(
      box(title = "Sentiment Positive vs Negative",
          solidHeader = T,
          background = "maroon",
          width = 12,
          collapsible = T,
          plotOutput("sentiment_contribution") %>% withSpinner(color="#1167b1")
      )
    ),
    fluidRow(
      tags$style(HTML('table.dataTable tr:nth-child(even) {background-color: #ffcfd8 !important;color: #3d3d3d !important;}}')),
      tags$style(HTML('table.dataTable tr:nth-child(odd) {background-color: #fff0f3 !important; color: #3d3d3d !important;}')),
      tags$style(HTML('table.dataTable thead tr td {color: black !important;}')),
      tags$style(HTML('table.dataTable th {background-color: #d41763 !important;}')),
      tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
            color: #303030 !important;
        }")),
      box(
        title = "Sentiment Analysis",
        background = "maroon",
        solidHeader = T,
        width = 12,
        collapsible = T,
        div(DT::dataTableOutput("table_review") %>% withSpinner(color="#1167b1"), style = "font-size: 90%;")
      ),
    ),
    fluidRow(
      box(title = "Word Cloud",
          background = "maroon",
          solidHeader = T,
          width = 6,
          collapsible = T,
          plotOutput("wordcloud") %>% withSpinner(color="#1167b1")
      ),
      box(title = "Word Count",
          background = "maroon",
          solidHeader = T,
          width = 6,
          collapsible = T,
          plotOutput("word_count") %>% withSpinner(color="#1167b1")
      )
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    withProgress({
      setProgress(message = "Collecting data", value = 0)
      
      result <- get_amusement_parks_reviews(input$url, input$size, incProgress)
    })
    
    return(result)
  })
  
  prediction_data <- reactive({
    withProgress({
      setProgress(message = "Predicting sentiment", value = 0)
      
      reviews <- data()$review
      incProgress(1/2)
      prediction <- predict_sentiment(reviews)
      incProgress(1/2)
    })
    prediction$reviewer <- data()$reviewer
    
    return(prediction)
  })
  
  create_dtm <- function(data) {
    corpus <- Corpus(VectorSource(data))
    
    corpus_clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    create_dtm <- corpus_clean %>%
      DocumentTermMatrix(control=list(dictionary = features))
  }
  
  dataWord <- reactive({
    v <- sort(colSums(as.matrix(create_dtm(data()$review))), decreasing = TRUE)
    data.frame(Kata=names(v), Jumlah=as.integer(v), row.names=NULL, stringsAsFactors = FALSE) %>%
      filter(Jumlah > 0)
  })
  
  output$total_review <- renderValueBox({
    valueBox(
      "Total", 
      paste0(nrow(prediction_data()), " review"),
      icon = icon("poll-h", class = "fas fa-poll-h", lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$positive_review <- renderValueBox({
    valueBox(
      "Positive", 
      paste0(nrow(prediction_data() %>% filter(sentiment == "Positive")), " review"),
      icon = icon("smile-beam", class = "fas fa-smile-beam", lib = "font-awesome"),
      color = "green")
  })
  
  output$negative_review <- renderValueBox({
    valueBox(
      "Negative",
      paste0(nrow(prediction_data() %>% filter(sentiment == "Negative")), " review"), 
      icon = icon("frown-open", class = "fas fa-frown-open", lib = "font-awesome"),
      color = "maroon")
  })
  
  # plot sentiment positive vs negative
  output$sentiment_contribution <- renderPlot({
    sentiments <- dataWord() %>% 
      inner_join(get_sentiments("bing"), by = c("Kata" = "word"))
    
    positive <- sentiments %>% filter(sentiment == "positive") %>% top_n(10, Jumlah) 
    negative <- sentiments %>% filter(sentiment == "negative") %>% top_n(10, Jumlah)
    sentiments <- rbind(positive, negative)
    
    sentiments <- sentiments %>%
      mutate(Jumlah=ifelse(sentiment =="negative", -Jumlah, Jumlah))%>%
      mutate(Kata = reorder(Kata, Jumlah))
    
    ggplot(sentiments, aes(Kata, Jumlah, fill=sentiment))+
      geom_bar(stat = "identity")+scale_fill_manual(values = c("#be1558", "#ff6684"))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ylab("Sentiment Contribution") + xlab("Word")
  })
  
  output$table_review <- renderDataTable(datatable({
    prediction_data()
  }, extensions = 'Buttons', options = list(dom = 'Bfrtip')))
  
  
  output$wordcloud <- renderPlot({
    data.corpus <- clean_data(data()$review)
    wordcloud(data.corpus, min.freq = 30, max.words = 50)
  })
  
  output$word_count <- renderPlot({
    countedWord <- dataWord() %>%
      top_n(10, Jumlah) %>%
      mutate(Kata = reorder(Kata, Jumlah))
    
    ggplot(countedWord, aes(Kata, Jumlah, fill = Jumlah)) + scale_fill_gradient(low="#ff6684", high="#be1558")+
      geom_col() +
      guides(fill = FALSE) +
      theme_minimal()+
      labs(x = NULL, y = "Word Count") +
      ggtitle("Most Frequent Words") +
      coord_flip()
  })
}

shinyApp(ui, server)