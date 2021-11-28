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

# --- UI ---
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




# --- SERVER --- 
library(shiny)
require(stringr)
library(sqldf)

twogramTable <- read.csv('twogramTable.csv')

server <- shinyServer(function(input, output) {
  output$inputText <- renderText({ input$txt })
  observeEvent(input$Submit, {
    txt <- gsub("\'","\'\'",input$txt)
    nwords <- str_count(txt, "\\S+")
    formattedTxt <- paste(unlist(strsplit(isolate(txt),' ')), collapse = '_')
    output$getPreds  <- renderPrint({
      if(nwords >= 5){
        print(getPreds(formattedTxt, twogramTable))
      }
      else{
        
        print(getPreds(formattedTxt, twogramTable))
      }
    })
  })
  

  getPreds <- function(x,k){
      t<- tolower(x)
      u<- paste(tail(unlist(strsplit(t,' ')),2), collapse=" ")
      v<- paste(tail(unlist(strsplit(t,' ')),1), collapse=" ")
      if (stri_count_words(x)>2){
        if (u %in% z$nminusgram){
          i <- z %>% filter(nminusgram==u) %>% .$lastword
          return(i[1])
        } else
          if (v %in% k$nminusgram){
          i <- k %>% filter(nminusgram==u) %>% .$lastword
          return(i[1])
        } else {return('the')}
      } else if(stri_count_words(x)==2){
        if (u %in% z$nminusgram){
          i <- z %>% filter(nminusgram==u) %>% .$lastword
          return(i[1])
        } else
        if (v %in% k$nminusgram){
          i <- k %>% filter(nminusgram==u) %>% .$lastword
          return(i[1])
        } else {return('the')}
      } else if(stri_count_words(x)==1){
        if (v %in% k$nminusgram){
          i <- k %>% filter(nminusgram==u) %>% .$lastword
          return(i[1])
        }else {return('the')}
      } else {print('wrong input')}
    }
})

shinyApp(ui = ui, server = server)




