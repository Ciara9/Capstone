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


# read in data
news_file <- file(paste0(getwd(),"/en_US/en_US.news.txt"), open="r")
news <- readLines(news_file)

blogs_file <- file(paste0(getwd(),"/en_US/en_US.blogs.txt"), open="r")
blogs <- readLines(blogs_file)

twit_file <- file(paste0(getwd(),"/en_US/en_US.twitter.txt"), open="r")
twitter <- readLines(twit_file)

#closing connections
close(news_file); close(blogs_file) ;close(twit_file)


size <- function(x){file.info(x)$size/1024/1024}
lines <- function(x){length(x)}
char <- function(x){sum(stri_length(x)-stri_count_fixed(x," "))}
words<- function(x){sum(stri_count_words(x))}
#creating summary file with the help of helper functions
filesummary<- data.frame(source=c("twitter","blogs","news"),
                         file_size=c(size("en_US/en_US.twitter.txt"),size("en_US/en_US.blogs.txt"),size("en_US/en_US.news.txt")),
                         no_of_lines=c(lines(twitter),lines(blogs),lines(news)),
                         no_of_characters=c(char(twitter),char(blogs),char(news)),
                         no_of_words=c(words(twitter),words(blogs),words(news)))
kable(filesummary, caption="Data summary")


#Reading 5000 lines data
twitter <- sample(twitter,4000, replace = FALSE)
blogs<- sample(blogs,4000, replace = FALSE)
news<- sample(news,4000, replace = FALSE)




data <- bind(twitter,blogs,news)


#removing non alphabets
data <- gsub("[^a-zA-Z ]","",data)

#creating corpus 
corpus<- VCorpus(VectorSource(data))

#corpus cleaning
corpus <- tm_map(corpus,removeNumbers)
corpus<- tm_map(corpus,removePunctuation)
corpus<- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))


#Create and plot the n-grams
unitoken<- function(x){NGramTokenizer(x, Weka_control(min = 1, max = 1))}
tdm_for_onegram <- TermDocumentMatrix(corpus,control = list(tokenize = unitoken))
fm <- rowSums(as.matrix(tdm_for_onegram))
onegram <- data.frame(words=names(fm),freq=fm)
onegram <- onegram[order(onegram$freq, decreasing=TRUE),]
top_onegram <- onegram[1:15,]
ggplot(top_onegram, aes(x=reorder(words,freq),y=freq))+geom_bar(stat="identity", fill="pink")+ geom_text(aes(y=freq+200,label=freq),vjust=1)+coord_flip()


bitoken <- function(x){NGramTokenizer(x, Weka_control(min=2, max=2))}
tdm_for_twogram <- TermDocumentMatrix(corpus,control=list(tokenizer = bitoken))
fm <- rowSums(as.matrix(tdm_for_twogram))
twogram <- data.frame(words=names(fm),freq=fm)
twogram <- twogram[order(twogram$freq, decreasing=TRUE),]
top_twogram <- twogram[1:15,]
ggplot(top_twogram, aes(x=reorder(words,freq),y=freq))+geom_bar(stat="identity", fill="purple")+ geom_text(aes(y=freq+10,label=freq),vjust=0.1)+coord_flip()

tritoken <- function(x){NGramTokenizer(x, Weka_control(min=3, max=3))}
tdm_for_threegram <- TermDocumentMatrix(corpus,control=list(tokenizer = tritoken))
fm <- rowSums(as.matrix(tdm_for_threegram))
threegram <- data.frame(words=names(fm),freq=fm)
threegram <- threegram[order(threegram$freq, decreasing=TRUE),]
top_threegram <- threegram[1:15,]
ggplot(top_threegram, aes(x=reorder(words,freq),y=freq))+geom_bar(stat="identity", fill="green")+ geom_text(aes(y=freq+1,label=freq),vjust=0.1)+coord_flip()

#Create n-gram table
createNgramTable <- function(x){
  z <- strsplit(as.character(x$words)," ")
  x$nminusgram <- NA
  x$lastword <- NA
  for (i in 1:nrow(x)){
    wo <- vector()
    for (j in 1:(length(z[[i]])-1)){
      wo <- c(wo,z[[i]][j])
    }
    x$nminusgram[i] <- paste(wo, collapse=" ")
    x$lastword[i] <- tail(z[[i]],1)
  }
  return(as.data.frame(x, row.names =NULL, stringsAsFactors=FALSE))
}
twogramTable <- createNgramTable(twogram)
threegramTable <- createNgramTable(threegram)

write.csv(twogramTable, 'twogramTable.csv')
write.csv(threegramTable, 'threegramTable.csv')


# model
prediction_model <- function(x,z,k){
  t<- tolower(x)
  u<- paste(tail(unlist(strsplit(t,' ')),2), collapse=" ")
  v<- paste(tail(unlist(strsplit(t,' ')),1), collapse=" ")
  if (stri_count_words(x)>2){
    if (u %in% z$nminusgram){
      i <- z %>% filter(nminusgram==u) %>% .$lastword
      return(i[1])
    } else if (v %in% k$nminusgram){
      i <- k %>% filter(nminusgram==u) %>% .$lastword
      return(i[1])
    } else {return('the')}
  } else if(stri_count_words(x)==2){
    if (u %in% z$nminusgram){
      i <- z %>% filter(nminusgram==u) %>% .$lastword
      return(i[1])
    } else if (v %in% k$nminusgram){
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


# TEST EXAMPLE
prediction_model("a way", threegramTable,twogramTable)











