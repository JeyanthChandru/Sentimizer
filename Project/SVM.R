library(RTextTools)
setwd("C:/xampp/htdocs/Project")

tweet <- read.csv('pre-processed.csv')


tweet.Shuffled<-tweet[sample(nrow(tweet)),]	#To shuffle the normalized dataframe
folds <- cut(seq(1,nrow(tweet.Shuffled)),10,labels=FALSE)
Maxacc <- array()
SVMacc <- array()
mat= create_matrix(tweet.Shuffled[,1], language="english", removeStopwords=FALSE, removeNumbers=TRUE, stemWords=FALSE, tm::weightTfIdf)
mat = as.matrix(mat)


for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)	#To create an index based on the folds
  trainIndexes <- which(folds!=i,arr.ind=TRUE)
  trainData <- tweet.Shuffled[-testIndexes, ]	#To generate the traindata based on the Negative Index
  testData <- tweet.Shuffled[testIndexes, ]

container = create_container(mat, as.numeric(as.factor(tweet.Shuffled[,2])), trainSize=trainIndexes, testSize=testIndexes,virgin=FALSE)
models = train_models(container, algorithms=c("MAXENT" , "SVM"), method = "C-classification")
results = classify_models(container, models)
senti <- as.numeric(testData$sentiment)
#table(senti, results$MAXENTROPY_LABEL)
Maxacc[i] <- recall_accuracy(senti,results$MAXENTROPY_LABEL)
#table(senti, results$SVM_LABEL)
SVMacc[i] <- recall_accuracy(senti,results$SVM_LABEL)
}

png(filename="maxenttrain.png", width=500, height=500)
plot(Maxacc,type='l',main=paste("ACCURACY OF MAXIMUM ENTROPY CLASSIFIER : ",mean(Maxacc)), xlab = 'NUMBER OF FOLDS', ylab = 'ACCURACY')
dev.off()
#acc_svm = cross_validate(container,N,"SVM")
png(filename="svmtrain.png", width=500, height=500)
plot(SVMacc,type='l',main=paste("ACCURACY OF SVM CLASSIFIER : ",mean(SVMacc)), xlab = 'NUMBER OF FOLDS', ylab = 'ACCURACY')
dev.off()

#cross_validate(container, N, "BAGGING")
#cross_validate(container, N, "NNET")

#SVM = train_model(container,"SVM")
#MAXENT = train_model(container,"MAXENT")
#result1 = classify_model(container,SVM)
#result2 = classify_model(container,MAXENT)
#analytics <- create_analytics(container,cbind(result1,result2))


#mat= create_matrix(tweet[,1], language="english", removeStopwords=FALSE, removeNumbers=TRUE, stemWords=FALSE, tm::weightTfIdf)
#container = create_container(mat, as.numeric(tweet$sentiment),trainSize=1:200, testSize=201:nrow(tweet),virgin=FALSE) 

#models = train_models(container, algorithms=c("MAXENT", "SVM","BAGGING", "NNET"))
#results = classify_models(container, models)
