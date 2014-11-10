####################################
# RFE parameters
####################################
library(ipred)
library(e1071)

#Custom Functions
glmnetFuncs <- caretFuncs #Default caret functions

glmnetFuncs$summary <-  twoClassSummary

glmnetFuncs$rank <- function (object, x, y) {
  vimp <- sort(object$finalModel$beta[, 1])
  vimp <- as.data.frame(vimp)
  vimp$var <- row.names(vimp)
  vimp$'Overall' <- seq(nrow(vimp),1)
  vimp
}

MyRFEcontrol <- rfeControl(
  functions = glmnetFuncs,
  method = "boot",
  number = 25,
  rerank = FALSE,
  returnResamp = "final",
  saveDetails = FALSE,
  verbose = TRUE)

####################################
# Training parameters
####################################
MyTrainControl=trainControl(
  method = "boot",
  number=25,
  returnResamp = "all",
  classProbs = TRUE,
  summaryFunction=twoClassSummary
)

####################################
# Setup Multicore
####################################
#source:
#http://www.r-bloggers.com/feature-selection-using-the-caret-package/
if ( require("multicore", quietly = TRUE, warn.conflicts = FALSE) ) {
  MyRFEcontrol$workers <- multicore:::detectCores()
  MyRFEcontrol$computeFunction <- mclapply
  MyRFEcontrol$computeArgs <- list(mc.preschedule = FALSE, mc.set.seed = FALSE)
  
  MyTrainControl$workers <- multicore:::detectCores()
  MyTrainControl$computeFunction <- mclapply
  MyTrainControl$computeArgs <- list(mc.preschedule = FALSE, mc.set.seed = FALSE)
}

x <- trainset[,-7]
y <- as.factor(trainset$Happy)
RFE <- rfe(x,y,sizes = seq(50,200,by=10),
           metric = "ROC",maximize=TRUE,rfeControl = MyRFEcontrol,
           method='rf',
           #tuneGrid = expand.grid(.sigma=0.5,.C=c(2^(-2:6))),
           trControl = MyTrainControl)

NewVars <- RFE$optVariables
RFE
plot(RFE)

FL <- as.formula(paste("Target ~ ", paste(NewVars, collapse= "+"))) #RFE


####################################
# Fit a GLMNET Model
####################################

model <- train(FL,data=trainset,method='glmnet',
               metric = "ROC",
               tuneGrid = expand.grid(.alpha=c(0,1),.lambda=seq(0,.25,by=0.005)),
               trControl=MyTrainControl)
model
plot(model, metric='ROC')
test <- predict(model, newdata=testset, type  = "prob")
