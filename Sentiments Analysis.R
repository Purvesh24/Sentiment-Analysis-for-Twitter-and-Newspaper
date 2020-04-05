install.packages("rtweet")
library (rtweet)
library(syuzhet)
library(ggplot2)


library(xlsx)
library (jsonlite)
library(dplyr)
library(syuzhet)
library(tidyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(reshape2)
library(reshape)
library(radarchart)
library("data.table")
library("CASdatasets")


api_key <- "kFelhgkZLCpERIGPPax0rsyvQ"
api_secret_key <- "uDqfyrqzWbyJuHPBWINXgvHDGBfrdyncIPpU2VTEFI41uQR0FL"
access_token <- "1478793870-wZg0jG8nCoB2fuKUd50SmO2SfE7KfON4x7Sfbff"
access_token_secret <- "qVsuvOUfKrnFlGEXaCW35RaWTo3gYGt9slcX6Z3Sr5COu"




## authenticate via web browser
token <- create_token(
  app = "Dublin City Bike",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)




DublinBikes <- search_tweets("Dublin_Bikes", n=1000, include_rts=FALSE, lang="en")


#Cleaning Dataset
DublinBikes$text <-  gsub("https\\S*", "", DublinBikes$text)
DublinBikes$text <-  gsub("@\\S*", "", DublinBikes$text) 
DublinBikes$text  <-  gsub("amp", "", DublinBikes$text) 
DublinBikes$text  <-  gsub("[\r\n]", "", DublinBikes$text)
DublinBikes$text  <-  gsub("[[:punct:]]", "", DublinBikes$text)



# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(DublinBikes$text, from="UTF-8", to="ASCII", sub="")
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

content <- fromJSON ("https://api.breakingapi.com/news?q=Dublin%20AND%20Bikes&type=everything&sources=independent.ie,herald.ie,irishnews.com,irishtimes.com,offalyindependent.ie&locale=en-IE&time_period=last_year&sort_by=date_published&output=json&page_size=100&api_key=AB6ADA837EB24C8D9390939837EB83A0",flatten = TRUE)
content <- as.data.frame(content)
#View(content)
NewsResponse <- content
#View(NewsResponse)

NewsResponse$response.results.webPublicationDate <- as.Date(NewsResponse$response.results.webPublicationDate)
Newsyear <- month(NewsResponse$articles.date_published)
#str(NewsResponse)

NewsResponse$articles.snippet <- trimws((gsub("<.*?>","",NewsResponse$articles.snippet)))

mysentiment_Classification <- get_nrc_sentiment(NewsResponse$articles.snippet)

head(mysentiment_Classification)


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  


ew_sentiment<-get_nrc_sentiment((DublinBikes$text))
head(ew_sentiment)

BothSentiments <- rbind(mysentiment_Classification,ew_sentiment)
head(BothSentiments)

sentimentscores<-data.frame(colSums(BothSentiments[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)

rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()
****************************************************************************
  
  
  
mysentiment_Classification_Radar <- data.frame(Newsyear,mysentiment_Classification)
View(mysentiment_Classification_Radar)


#Sentiment_ID <- seq(1:nrow(mysentiment_Classification))
#View(mysentiment_Classification)
#mysentiment_Classification <- cbind(Sentiment_ID, mysentiment_Classification)
#head(mysentiment_Classification)

MoltenSentiments <- melt(mysentiment_Classification_Radar, id=c("Newsyear"))
head(MoltenSentiments,id=3)

abc <- aggregate(value ~ variable+Newsyear, MoltenSentiments, sum)

View(abc)

RR <- reshape(data=abc,idvar="variable",
              v.names = "value",
              timevar = "Newsyear",
              direction="wide")


head(RR)

colnames(RR) <- c("Sentiments","Year1","Year2","Year3")
RR %>%
  chartJSRadar(showToolTipLabel = TRUE,
               main = "NRC Years Radar")

  