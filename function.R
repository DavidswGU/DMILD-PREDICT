library(caret)
library(pROC)
#custom function
auroc <- function(x,y){
  roc <- roc(y,x)
  return(roc$auc)
}

confmat <- function(pred,ob){
  confusion_matrix <- confusionMatrix(pred, ob)
  accuracy <- confusion_matrix$overall["Accuracy"]
  precision <- confusion_matrix$byClass["Pos Pred Value"]
  recall <- confusion_matrix$byClass["Sensitivity"]
  f1_score <- confusion_matrix$byClass["F1"]
  print(confusion_matrix)
  print(paste("Accuracy:", accuracy))
  print(paste("Precision:", precision))
  print(paste("Recall:", recall))
  print(paste("F1 Score:", f1_score)) 
  return(confusion_matrix)
}
refactor <- function(x){
  for (i in 1:ncol(x)) {
      if("Deceased"%in%x[,i]==TRUE){
        levels(levels(x[,i]))[2] <- 0
        levels(levels(x[,i]))[1] <- 1
      }
      if("Yes"%in%x[,i]==TRUE){
        levels(levels(x[,i]))[2] <- 1
        levels(levels(x[,i]))[1] <- 0
      }
  }
  return(x)
}

