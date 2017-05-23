setwd("C:/xampp/htdocs/Project")
graphics.off()
rm(list=ls() )
cat("\014")

library(twitteR)

#search_key <- readline(prompt="Enter a Search Key: ")
args = commandArgs(trailingOnly=TRUE)
search_key <- args[1]
api_key <- "0TlACUB2Fu6SVU3Mg32QL84jh"
api_secret <- "rZ1MWy7yygmAISMFwROg99EXBP0GByBDT6fABG9Jf9bdz1KPua"
access_token <- "184791318-S46swosuYjac9RimfSyBZLJE18OJT5tyx7fkP5Pe"
access_token_secret <- "lDJIOFEcjfyyYFIJuwKZ2U561uFODM9npzl4PNW6tkFTp"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
tweets <- searchTwitter(search_key,n=1000,lang = "en")
tweets <- strip_retweets(tweets)
tweet = sapply(tweets, function(x) x$getText())
tweet <- data.frame(tweet)
names(tweet) <- c("content")
write.csv(tweet,'text_twee.csv',row.names = FALSE)
#write.arff(tweet,'text_twee.arff')
