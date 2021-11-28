library(shiny)
library(stringi)
library(ggplot2)
library(magrittr)
library(markdown)
library(knitr)
library(RWeka)
library(openNLP)
library(wordcloud)
library(tm)
library(NLP)
library(qdap)
library(RColorBrewer)
library(dplyr)


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Next Word Prediction", "Data Science Capstone Project"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("txt", label = "Enter text here", value = "", width = NULL, placeholder = NULL),
      h5('Please press \'Submit\' to see the prediction results.'),
      actionButton("Submit","Submit")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3('Predicted Word'),
      h5('The objective of this app is to predict the word most likely to come after your chosen word.'),
      h4('You entered'),
      verbatimTextOutput("inputText"),
      h4('Next predicted word'),
      verbatimTextOutput("getPreds")
    )
  )
))

