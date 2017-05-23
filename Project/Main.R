#list.of.packages <- c("tm", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) 
#  install.packages(new.packages)
#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

library(qdapRegex)
library(sampling)
require(tm)
#require(igraph)
#require(fpc)
#require(Rcampdf)
#require(stringr)
#library(RTextTools)
#library(e1071)
#library(sampling)
#library(quanteda)
#require(twitteR)

setwd("C:/xampp/htdocs/Project")

positive.words <- read.csv("positive-words.csv")
negative.words <- read.csv("negative-words.csv")
Neu <- read.csv("neutral-words.csv")
calculate_scores<-function(text){
  text<-as.character(text)
  text <- unlist(lapply(text, function(x) { stringr::str_split(x, "\n") })) 
  sentence <- preprocess(text)
  getpolarity <- function(sentences, negative_words,positive_words,neutral_words){
    polaritys <- plyr::laply(sentences, function(sentence, negative_words,positive_words, neutral_words){
      wordList <- stringr::str_split(sentence, '\\s+')
      words <- unlist(wordList)
      positive.matches <-match(words, positive_words)
      negative.matches <-match(words, negative_words)
      neutral.matches <- match(words,neutral_words)
      positive_matches <-!is.na(positive.matches)
      negative_matches <-!is.na(negative.matches)
      neutral_matches <- !is.na(neutral.matches)
      score <-sum(positive_matches) - sum(negative_matches)
      return(score)
    }, negative_words, positive_words, neutral_words)
    
    return(polaritys)
  }    
  negative_words<- negative.words$content
  positive_words<- positive.words$content
  neutral_words <- Neu$content
  
  #build tables of positive and negative sentences with polaritys
  negative_words <- tolower(negative_words)
  positive_words <- tolower(positive_words)
  neutral_words <- tolower(neutral_words)
  
  res<-getpolarity(text,negative_words,positive_words,neutral_words)
  return (res)
}

calculate_sentiment<-function(text)
{
  res<-calculate_scores(text)
  sentiment<-c()
  for(i in 1 : length(res))
  {
    if(res[i] < -10 && res[i] > 10)
    {
      sentiment[i]<-'Sarcasm'
    }
    else
    {
      if(res[i] > -1 && res[i] < 1)
      {
        sentiment[i]<-'Neutral'
      }
      else if (res[i] >= 1 && res[i] <= 10){
        sentiment[i]<-'Positive'
      }
      else{
        sentiment[i]<-'Negative'
      }
    }
  }
  results<-data.frame(text,sentiment)
  return (results)
}

calculate_total_presence_sentiment<-function(text){
  res<-calculate_scores(text)
  score_array<-array(0,dim=c(2,4))
  score_array[1,1]<-'Sarcasm'
  score_array[1,2]<-'Neutral'
  score_array[1,3]<-'Positive'
  score_array[1,4]<-'Negative'
  
  for(i in 1 : length(res))
  {
    if(res[i] < -10 && res[i] > 10)
    {
      
      score_array[2,1]<-as.numeric(score_array[2,1])+1
    }
    else
    {
      if(res[i] > -1 && res[i] < 1)
      {
        
        score_array[2,2]<-as.numeric(score_array[2,2])+1
        
      }
      else if (res[i] >= 1 && res[i] <= 10){
        
        score_array[2,3]<-as.numeric(score_array[2,3])+1
      }
      else {
        score_array[2,4]<-as.numeric(score_array[2,4])+1
      }
    }
  }
  return (score_array)
}
preprocess = function(x)
{
  rm_twitter_url <- rm_(pattern=pastex("@rm_twitter_url", "@rm_url"))
  x <- rm_twitter_url(x)
  #x = x[!duplicated(x)]
  x = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", x)
  x = gsub("@\\w+", "",x)
  x = gsub("[[:punct:]]","", x)
  x = gsub("[[:cntrl:]]", "", x)
  x = gsub("[[:digit:]]","",x)
  x = gsub("[ \t]{2,}","",x)
  x = gsub("^\\s+|\\s+$","",x)
  x = gsub("[^[:alnum:]]"," ",x)
  x = gsub("^ ", "", x)  # remove blank spaces at the beginning
  x = gsub(" $", "", x) # remove blank spaces at the end
  x = gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", x) # Remove 1-2 letter words
  x = gsub("^ +| +$|( ) +", "\\1", x)
  x = gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", x)
  #x = gsub('([[:alpha:]])\\1+', '\\1', x)
  return(x)
}
tweet <- read.csv("text_twee.csv")
#samp <- srswor(1000,length(tweet$sentiment))
#tweet <- tweet[which(samp==1),]
tweet[,1] <- preprocess(tweet[,1])
docs <- Corpus(VectorSource(tweet[,1]))
docs <- tm_map(docs, removeWords, Neu$content[1:3000])
docs <- tm_map(docs, removeWords, Neu$content[3001:4922])
tweet[,1] <- data.frame(text = sapply(docs, as.character), stringsAsFactors = FALSE)
tweet[,1] <- preprocess(tweet[,1])
#empty = which(tweet$content=='')
#tweet <- data.frame(tweet[-empty,])
sent <- calculate_sentiment(tweet[,1])
png(filename="sentiplot.png", width=500, height=500)
plot(sent$sentiment, col="lightblue", main=paste('Number of Instances : ',nrow(sent)), xlab = 'Sentiment', ylab = 'Reviews by Tweet')
dev.off()
names(sent) <- c('content','sentiment')
write.csv(sent,"pre-processed.csv",row.names = FALSE)

#conf.mat = table(tweet$sentiment,sent$sentiment)
#acc = sum(diag(conf.mat))/sum(conf.mat)

#pre = array()
#rec = array()

#for(i in 1:3){
#  for(j in 1:3){
#    if(i==j){
#      pre[i] = conf.mat[i,j]/sum(conf.mat[,j])
#      rec[i] = conf.mat[i,j]/sum(conf.mat[j,])
#    }
#  }
#}

#Fscore = (2*(pre*rec)) / (pre + rec)

