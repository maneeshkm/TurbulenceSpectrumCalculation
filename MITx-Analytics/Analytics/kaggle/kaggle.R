
library(mice)
library(rpart)
library(randomForest)
library(caTools)
library(ROCR)
library(caret)
# library(gbm)
# library(ipred)
library(e1071)
set.seed(100)
train = read.csv("train.csv")
yob = mice(train[,1:8])
yob1 = complete(yob)
train$UserID = NULL
train$YOB = yob1$YOB
train$Happy <- as.factor(train$Happy)


var_names_train = names(train)
for (i in var_names_train[8:length(var_names_train)]) {
  levels(train[,i]) <- c(levels(train[,i]), "Skipped")
  train[,i][train[,i]==''] = "Skipped"
}

train$votes = as.numeric(train$votes)
#train$votes = as.factor(train$votes)
split = sample.split(train$Happy, SplitRatio=0.8)
trainset = subset(train, split==TRUE)
testset = subset(train, split=FALSE)

# MyRFEcontrol <- rfeControl(
#   functions = glmnetFuncs,
#   method = "boot",
#   number = 25,
#   rerank = FALSE,
#   returnResamp = "final",
#   saveDetails = FALSE,
#   verbose = TRUE)

#Model for classification
# logistic.fit = glm(Happy ~ . - UserID, data = trainset, family = 'binomial'  )
# cart.fit = rpart(Happy ~ ., data=trainset, method="class",control=rpart.control(minbucket=100))
set.seed(123)

#Feature selection using LDA
# slda <- train(Happy ~ ., data = trainset,
#               method = "stepLDA",
#               trControl = trainControl(method = "cv"))

# rf.fit = randomForest(Happy ~ . , data=trainset, ntree=250,mtry=20, do.trace=T)
# tc <- trainControl(method="cv",summaryFunction=twoClassSummary,classProb=T)
#tc <- trainControl("repeatedcv", number=1, repeats=1, classProbs=TRUE, savePred=T) 
# RFFit <- train(Happy ~., data=trainset, method="rf")
# train.rf <- train(Happy ~ .,data=trainset, method="rf")
#gb.fit = gbm(Happy ~ ., distribution='gaussian',data=trainset, n.trees=200)
#tuneRF( train[,-8], train[,8], mtryStart=10, improve=0.1, ntreeTry=50, stepFactor=2, doBest=TRUE)
cf.fit = cforest(Happy ~. , data=trainset)
#rf.fit = train(trainset[,-7], trainset[,7], method='svmRadial', sigma=0.5)
pred = predict(cf.fit, newdata=trainset)
table((pred), trainset$Happy)

pred = predict(cf.fit, newdata=testset)
table((pred), testset$Happy)

ROCRpred = prediction(pred@.Data -1 , testset$Happy)
print("ROCR")
as.numeric(performance(ROCRpred, "auc")@y.values)

test = read.csv("test.csv")
var_names_test = names(test)
for (i in var_names_test[8:length(var_names_test)]) {
  levels(test[,i]) <- c(levels(test[,i]), "Skipped")
  test[,i][test[,i]==''] = 'Skipped'
}
yob2 = mice(test[,1:7])
yob3 = complete(yob2)
test$YOB = yob3$YOB
user = test$UserID
test$UserID = NULL
testPred = predict(rf.fit, newdata=test, type='prob')

#submission
submission = data.frame(UserID = user, Probability1 = testPred[,2])
write.csv(submission, "submission.csv", row.names=FALSE) 

