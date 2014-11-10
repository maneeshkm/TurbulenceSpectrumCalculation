library(tm)
library(rpart)
library(caTools)
library(rpart.plot)

clinicalTrials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE, fileEncoding='latin1')

corpusTitle = Corpus(VectorSource(clinicalTrials$title))

# Pre-process data
corpusTitle <- tm_map(corpusTitle, tolower)
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle <- tm_map(corpusTitle, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))

corpusAbstract = Corpus(VectorSource(clinicalTrials$abstract))

corpusAbstract <- tm_map(corpusAbstract, tolower)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, stemDocument)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = clinicalTrials$trial

set.seed(144)

split = sample.split(dtm$trial, SplitRatio=0.7)
train = subset(dtm, split==TRUE)
test = subset(dtm, split==FALSE)

trialCART = rpart(trial ~ . , data=train, method='class')
prp(trialCART)

pred = predict(trialCART, newdata=test)
pred.prob = pred[,2]

library(ROCR)
predROCR = prediction(pred.prob, test$trial)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values
