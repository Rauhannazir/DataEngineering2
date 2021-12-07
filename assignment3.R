rm(list=ls())

getwd()
setwd("C:/Users/Rauhan Nazir/Documents/R_Codes/ceu-cloud-class/serverless")

install.packages("aws.comprehend", repos = c(cloudyr = "http://cloudyr.github.io/drat", getOption("repos")))
install.packages("wordcloud")
install.packages("RColorBrewer")
isntall.packages("tm")
library(rvest)
library(xml2)
library(dplyr)
library(data.table)
library(httr)
library("aws.comprehend")
library(ggthemes)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(tidyverse)
library(aws.comprehend)
library(esquisse)
library(plotrix)
library(wordcloud2)
library(RColorBrewer)
library(tm)




# Set up your R w/ AWS

keyfile = list.files(path=".", pattern="accessKeys.csv", full.names=TRUE)
if (identical(keyfile, character(0))){
  stop("ERROR: AWS key file not found")
}
keyTable <- read.csv(keyfile, header = T)
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)
#activate
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1")


# Web Scraping 
# THE GUARDIAN
tg<- read_html("https://www.theguardian.com/world/2021/feb/23/china-did-little-hunt-covid-origins-early-months-says-who-document")
description <- tg %>% html_nodes('#dfp-ad--inline3+ .dcr-o5gy41 , .dcr-1mfia18+ .dcr-o5gy41 , .dcr-o5gy41:nth-child(16) , #dfp-ad--inline2+ .dcr-o5gy41 , .dcr-o5gy41:nth-child(10) , .dcr-10khgmf+ .dcr-o5gy41 , .dcr-o5gy41:nth-child(7) , .dcr-o5gy41:nth-child(6) , .dcr-o5gy41:nth-child(5) , #sign-in-gate+ .dcr-o5gy41 , .dcr-o5gy41:nth-child(2) , .dcr-125vfar , .dcr-o5gy41:nth-child(1)') %>% html_text()
detect_language(description)
the_guardian <- detect_sentiment(description)
the_guardian
the_guardian$Sentiment <- NULL
guardian_news <- the_guardian %>% gather("sentiment", "score", -1)


ggplot(data = guardian_news ,aes(x= Index, y = score ,fill = sentiment))+
  geom_bar(stat="identity", position ="fill")+
  ggtitle("THE GUARDIAN Newspaper")+
  theme_economist()


#For the whole article combined 

gc <- paste0(description,collapse = "")
gc_sent <- detect_sentiment(gc)
gc_sent$Sentiment <- NULL
gc_comb <- gc_sent %>% gather("sentiment", "score", -1)



pie3D(gc_comb$score,
      col = hcl.colors(length(gc_comb$score), "Spectral"),
      labels = gc_comb$sentiment,labelcex=1.1, labelrad=1.4,theta=1,radius =1,
      explode=0.2 , main="The Guardian Article Analysis")



# Science
s <- read_html("https://www.science.org/content/article/claim-chinese-team-hid-early-sars-cov-2-sequences-stymie-origin-hunt-sparks-furor")
description2 <- s %>% html_nodes('.bodySection p , strong') %>% html_text()
detect_language(description2)
sa <- detect_sentiment(description2)
sa$Sentiment <-NULL
# Long to Wide Format
science <- sa %>% gather("sentiment", "score", -1)


# Graphics Science
ggplot(data = science,aes(x= Index, y = score ,fill = sentiment))+
  geom_bar(stat="identity", position ="fill")+
  ggtitle("Science Article")+
  theme_economist()





# word_cloud_Guardian

docs <- Corpus(VectorSource(paste(description, collapse = '')))
inspect(docs)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d <- d[-c(1,2,3,4,5),]
d
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=180, random.order=TRUE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

# word-cloud Science

docs1 <- Corpus(VectorSource(paste(description1, collapse = '')))
inspect(docs1)
dtm1 <- TermDocumentMatrix(docs1)
m <- as.matrix(dtm1)
v1 <- sort(rowSums(m),decreasing=TRUE)
d1 <- data.frame(word = names(v1),freq=v1)
head(d1, 10)
d1 <- d1[-c(1,3,4,7,8),]
set.seed(1234)
wordcloud(words = d1$word,freq = d1$freq, min.freq = 1,
          max.words=180, random.order=TRUE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


