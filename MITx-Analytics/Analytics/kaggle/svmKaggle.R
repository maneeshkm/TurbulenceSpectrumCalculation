library(kernlab)
sigDist <- sigest(Happy ~ ., data = trainset, frac = 1)
### creating a grid of two tuning parameters, .sigma comes from the earlier line. we are trying to find best value of .C
svmTuneGrid <- data.frame(.sigma = sigDist[1], .C = 2^(-2:6))

set.seed(1056)
svmFit <- train(as.factor(Happy) ~ . ,
                data = trainset,
                method = "svmRadial",
                preProc = c("center", "scale"))
                #tuneGrid = svmTuneGrid,
                #metric = "Kappa",
                #trControl = trainControl(method = "repeatedcv", repeats = 5))

# svmFit <- ksvm(Happy ~ . , data=trainset, type='C-svc',kernel='rbfdot', sigma=0.1, C=20, prob.model=T, cross=5)

pred = predict(rf.fit, newdata=trainset)
table((pred), trainset$Happy)

pred = predict(svmFit, newdata=testset, type='response')
table(pred, testset$Happy)

ROCRpred = prediction(pred@.Data -1 , testset$Happy)
print("ROCR")
as.numeric(performance(ROCRpred, "auc")@y.values)

test = read.csv("test.csv")
var_names_test = names(test)
for (i in var_names_test[8:length(var_names_test)]) {
  levels(test[,i]) <- c(levels(test[,i]), "Skipped")
  test[,i][is.na(test[,i])] = 'Skipped'
}
yob2 = mice(test)
yob3 = complete(yob2)
test$YOB = yob3$YOB
user = test$UserID

testPred = predict(svmFit, newdata=test, type='prob')

#submission
submission = data.frame(UserID = user, Probability1 = testPred[,2])
write.csv(submission, "submission.csv", row.names=FALSE) 

