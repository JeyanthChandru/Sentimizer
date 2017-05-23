library(RTextTools)
library(tm)
library(plyr)
setwd("C:/xampp/htdocs/Project")
#set.seed(1234)
tweet <- read.csv('pre-processed.csv')
#tweet <- read.csv('text_emotion.csv')
library(foreign)
write.arff(tweet,'tweet.arff')

calc_prob <- function(words_df,prob_freq){
  prob_sum = array()
  for(k in 1:nrow(words_df)){
    count <- which(as.character(prob_freq$content) == words_df[k,])
    if(length(count)>0)
      prob_sum[k] <- prob_freq$Prob[count]
    else
      prob_sum[k] <- 0
  }
  pro_val = sum(prob_sum)
  return(pro_val)
}
calc_probability <- function(Test_Test,prob_val,prob_freq){
  Prob_arr = array()
  for(i in 1:nrow(Test_Test)){
    List <- strsplit(as.character(Test_Test[i,1]), " ")
    Test_Set_Words <- data.frame( content=unlist(List))
    for(j in 1:nrow(Test_Set_Words)){
      Prob_arr[i] <- prob_val * calc_prob(Test_Set_Words,prob_freq)
    }
  }
  return(Prob_arr)
}

Calculate_Freq <- function(Cal_Words){
  myCorpus <- Corpus(VectorSource(Cal_Words$content))
  temp <- slam::row_sums(TermDocumentMatrix(myCorpus))
  #myTdm <- TermDocumentMatrix(myCorpus)
  #temp <- inspect(myTdm)
  #FreqMat <- data.frame(content = rownames(temp), Freq = rowSums(temp))
  FreqMat <- data.frame(names(temp))
  FreqMat$Freq <- temp
  names(FreqMat) <- c("content","Freq")
  row.names(FreqMat) <- NULL
  return(FreqMat)
}
#Pos <- read.csv('positive-words.csv')
#Neg <- read.csv('negative-words.csv')
#Neu <- read.csv('neutral-words.csv')

folds <- split(tweet, cut(sample(1:nrow(tweet)),10))
acc <- array()
for (z in 1:length(folds)) {
  test <- ldply(folds[z], data.frame)
  train <- ldply(folds[-z], data.frame)
  List <- strsplit(as.character(train$content), " ")
  Data_Set_Words <- data.frame( content=unlist(List),sentiment=rep(train$sentiment, sapply(List, length)))
  names(Data_Set_Words) <- c("content","sentiment")
  
  Neu_Words <- Data_Set_Words[which(Data_Set_Words$sentiment == "Neutral"),]
  Neg_Words <- Data_Set_Words[which(Data_Set_Words$sentiment == "Negative"),]
  Pos_Words <- Data_Set_Words[which(Data_Set_Words$sentiment == "Positive"),]
  #Neu_Words <- rbind(Neu_Words,Neu)
  #Neg_Words <- rbind(Neg_Words,Neg)
  #Pos_Words <- rbind(Pos_Words,Pos)
  Neu_Freq <- Calculate_Freq(Neu_Words)
  Neg_Freq <- Calculate_Freq(Neg_Words)
  Pos_Freq <- Calculate_Freq(Pos_Words)
  Total_Words <- rbind(Neu_Words,Neg_Words,Pos_Words)
  prob_neutral = length(which(Total_Words$sentiment == "Neutral"))/nrow(Total_Words)
  prob_pos = length(which(Total_Words$sentiment == "Positive"))/nrow(Total_Words)
  prob_neg = length(which(Total_Words$sentiment == "Negative"))/nrow(Total_Words)
  #prob_neutral <- length(which(train$sentiment == "Neutral"))/nrow(train)
  #prob_pos <- length(which(train$sentiment == "Positive"))/nrow(train)
  #prob_neg <- length(which(train$sentiment == "Negative"))/nrow(train)
  
  Pos_Prob <- array()
  for(i in 0:nrow(Pos_Freq)){
    Pos_Prob[i] = (Pos_Freq$Freq[i] + 0.1)/(nrow(Pos_Freq) + nrow(Pos_Words))
  }
  Neg_Prob <- array()
  for(i in 0:nrow(Neg_Freq)){
    Neg_Prob[i] = (Neg_Freq$Freq[i] + 0.1)/(nrow(Neg_Freq) + nrow(Neg_Words))
  }
  Neu_Prob <- array()
  for(i in 0:nrow(Neu_Freq)){
    Neu_Prob[i] = (Neu_Freq$Freq[i] + 0.1)/(nrow(Neu_Freq) + nrow(Neu_Words))
  }
  Pos_Freq$Prob <- Pos_Prob
  Neg_Freq$Prob <- Neg_Prob
  Neu_Freq$Prob <- Neu_Prob
  
  Positive_Prob <- calc_probability(as.data.frame(test$content),prob_pos,Pos_Freq)
  Negative_Prob <- calc_probability(as.data.frame(test$content),prob_neg,Neg_Freq)
  Neutral_Prob <- calc_probability(as.data.frame(test$content),prob_neutral,Neu_Freq)
  Final_Prob <- data.frame(Positive_Prob,Negative_Prob,Neutral_Prob)
  Predicted = array()
  for(i in 1:nrow(test)){
    if(Positive_Prob[i] > Negative_Prob[i]){
      if(Positive_Prob[i] > Neutral_Prob[i]){
        Predicted[i] <- "Positive"
      }
      else
        Predicted[i] <- "Neutral"
    }
    else{
      if(Negative_Prob[i] > Neutral_Prob[i]){
        Predicted[i] <- "Negative"
      }
      else
        Predicted[i] <- "Neutral"
    }
  }
  conf.mat <- table(Predicted,test$sentiment)
  acc[z] <- recall_accuracy(Predicted,test$sentiment)
  print(conf.mat)
}
png(filename="nb.png", width=500, height=500)
plot(acc, col="blue", main=paste('Accuracy of Naive Bayes : ',mean(acc)), xlab = 'Folds', ylab = 'Accuracy');lines(acc,col="blue")
dev.off()

