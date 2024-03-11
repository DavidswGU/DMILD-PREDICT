library(mlr)
library(GenSA)
library(caTools)
library(caret)
library(survminer)
library(survival)
set.seed(58)
#custom function
perf <- function(model,data,target,prob.thres){
  test_predictions <- predict(model, newdata = data, type="prob")
  test_predictions <- as.data.frame(test_predictions)
  test_predictions <- test_predictions%>% mutate(level = ifelse(test_predictions$'1'>=prob.thres, 1, 0))
  test_predictions <- as.factor(test_predictions$level)
  confusion_matrix <- confusionMatrix(test_predictions, target)
  accuracy <- confusion_matrix$overall["Accuracy"]
  precision <- confusion_matrix$byClass["Pos Pred Value"]
  recall <- confusion_matrix$byClass["Sensitivity"]
  f1_score <- confusion_matrix$byClass["F1"]
  print(confusion_matrix)
  print(paste("Accuracy:", accuracy))
  print(paste("Precision:", precision))
  print(paste("Recall:", recall))
  print(paste("F1 Score:", f1_score)) 
}
#make data
train.select.pred=train.select.data[,-ncol(train.select.data)]
train.select.target=as.factor(train.select.data[,ncol(train.select.data)])
#mlr
task <- makeClassifTask(data = train.select.data,target = "surv")
learner <- makeLearner("classif.randomForest",predict.type = "response")
params <- makeParamSet(
  makeIntegerParam("mtry",lower=2,upper = getTaskNFeats(task)),
  makeIntegerParam("ntree",lower=10, upper=1000)
)
resamp <- makeResampleDesc("CV",iters = 5)
measure <- mmce
tune <- makeTuneControlGenSA(maxit=100)
tuned <- tuneParams(learner,task,resamp,measure,params,tune)
print(tuned)
#using tuned params
model.tuned <- randomForest(x=train.select.pred,y=train.select.target,mtry=2,ntree=452,importance = TRUE)
test.select.target <- as.factor(test.select.data[, ncol(test.select.data)])
perf(model.tuned,data=train.select.data,target = train.select.target,prob.thres=0.5)
perf(model.tuned,data=test.select.data,target = test.select.target,prob.thres=0.5)
perf(randomForest(x=train.pred,y=train.target),
     data=test.data,target = as.factor(test.data[, ncol(test.data)]),prob.thres=0.5)
test_predictions <- predict(model.tuned, newdata = test.select.data, type="prob")
test_predictions <- as.data.frame(cbind(test_predictions[,2],test.select.data$surv))
roccurve <- roc(test_predictions$V2,test_predictions$V1)
plot(roccurve,
     legacy.axes = TRUE,
     print.thres="best")
#prepare data for DCA
test_predictions <- as.data.frame(test_predictions)
rf.test.dca <- test_predictions$'1'
