library(caret)
library(pROC)
library(scales)

#function for calculating auroc without conf.int
auroc <- function(x,y){
  roc <- roc(y,x)
  return(roc$auc)
}

#function for returning metrics and confusion matrix
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

#function for percentage conversion
pct <- function(x){
  x <- round(x,4)*100
  x <- paste0(x,"%")
  return(x)
}

#function for identifying binary variables
is.binary <- function(x){
  table <- data.frame(table(x))
  if(nrow(table)==1) return("single") 
  if(nrow(table)==2) return("binary")
  if(nrow(table)>2) return("other")
}

#function for comparing binary variables in two groups
binarycomp <- function(x1,x2){
  np=0
  pp=0
  pn=0
  nn=0
  Diff.rest <- as.data.frame(matrix(nrow = 1,ncol = 7))
  n = 3
  pp <- as.data.frame(table(x1))[2,2]
  pn <- as.data.frame(table(x1))[1,2]
  np <- as.data.frame(table(x2))[2,2]
  nn <- as.data.frame(table(x2))[1,2]
  if(is.na(np)==TRUE) np=0
  if(is.na(pp)==TRUE) pp=0
  if(is.na(pn)==TRUE) pn=0
  if(is.na(nn)==TRUE) nn=0
  mtx.test <- matrix(c(pp,pn,np,nn),nrow = 2)
  if(pp<=5|pn<=5|nn<=5|np<=5) {
    if((pp+pn+np+nn)>0) {
      n=0
    } 
  } else n=1
  if(n==1)
  {
    pval <- chisq.test(mtx.test,correct = FALSE)$p.value
    stat <- "Chi-Square"
  }
  if(n==0)
  {
    pval <- fisher.test(mtx.test)$p.value
    stat <- "Fisher"
  }
  Diff.rest[,1] <- paste0("'=",as.data.frame(table(x1))[2,1])
  Diff.rest[,2] <- paste0(pct((pp+np)/(pp+pn+np+nn))," (n=",pp+np,")")
  Diff.rest[,3] <- paste0(pct(pp/(pp+pn))," (n=",pp,")")
  Diff.rest[,4] <- paste0(pct(np/(np+nn))," (n=",np,")")
  Diff.rest[,5] <- stat
  if(pval>=0.001) Diff.rest[,6] <- as.character(round(pval,3)) else Diff.rest[,6] <- "<0.001"
  Diff.rest[,7] <- ""
  if(pval<0.05) Diff.rest[,7] <- "*"
  if(pval<0.01) Diff.rest[,7] <- "**"
  if(pval<0.001) Diff.rest[,7] <- "***"
  return(Diff.rest)
}

#function for comparing continuous variables between groups
contcomp <- function(x1,x2){
  Diff.rest <- as.data.frame(matrix(nrow = 1,ncol = 7))
  n = 0
  xnorm <- ks.test(x1,pnorm,exact = TRUE)$p.value
  ynorm <- ks.test(x2,pnorm,exact = TRUE)$p.value
  if(xnorm>0.05&ynorm>0.05) n=1
  if(n==1)
  {
    pval <- t.test(x1,x2,var.equal = TRUE,conf.level = 0.95)$p.value
    stat <- "student's t"
  }
  if(n==0)
  {
    pval <- ks.test(x1,x2)$p.value
    stat <- "K-S"
  }
  Diff.rest[,1] <- "Continuous"
  Diff.rest[,2] <- paste0(median(c(x1,x2),na.rm = TRUE),"("
                          ,quantile(c(x1,x2),probs = 0.25,na.rm = TRUE),","
                          ,quantile(c(x1,x2),probs = 0.75,na.rm = TRUE),")")
  Diff.rest[,3] <- paste0(round(median(x1,na.rm = TRUE),2),"("
                          ,quantile(x1,probs = 0.25,na.rm = TRUE),","
                          ,quantile(x1,probs = 0.75,na.rm = TRUE),")")
  Diff.rest[,4] <- paste0(median(x2,na.rm = TRUE),"("
                          ,quantile(x2,probs = 0.25,na.rm = TRUE),","
                          ,quantile(x2,probs = 0.75,na.rm = TRUE),")")
  Diff.rest[,5] <- stat
  if(pval>=0.001) Diff.rest[,6] <- as.character(round(pval,3)) else Diff.rest[,6] <- "<0.001"
  Diff.rest[,7] <- ""
  if(pval<0.05) Diff.rest[,7] <- "*"
  if(pval<0.01) Diff.rest[,7] <- "**"
  if(pval<0.001) Diff.rest[,7] <- "***"
  return(Diff.rest)
}

# a function for comparing data between groups, combining the two functions above
comp <- function(x,sep){
  sepnum <- which(colnames(x)==sep)
  table <- data.frame(table(x[,sepnum]))
  sep1 <- table[1,1]
  sep2 <- table[2,1]
  x1 <- subset(x,x[,sepnum]==sep1)[,-sepnum]
  x2 <- subset(x,x[,sepnum]==sep2)[,-sepnum]
  var.list <- c(colnames(x1))
  Diff.res <- as.data.frame(matrix(nrow = 1,ncol = 7))
  for (i in 1:ncol(x1)) {
    x1.test <- x1[,i]
    x2.test <- x2[,i]
    if (is.binary(x1[,i])=="binary"|is.binary(x2[,i])=="binary") Diff.rest <- binarycomp(x1.test,x2.test) 
    if (is.binary(x1[,i])=="other"|is.binary(x2[,i])=="other") Diff.rest <- contcomp(x1.test,x2.test)
    if (is.binary(x1[,i])=="single"&is.binary(x2[,i])=="single") Diff.rest <- as.data.frame(c("Single-value","100%","100%","100%","NA","1",""))
    Diff.res <- rbind(Diff.res,Diff.rest)
  }
  Diff.res <- Diff.res[-1,]
  Diff.res <- cbind(colnames(x1),Diff.res)
  xname <- paste0("Total"," (n=",nrow(x),")")
  x1name <- paste0(sep,"=",table[1,1]," (n=",nrow(x1),")")
  x2name <- paste0(sep,"=",table[2,1]," (n=",nrow(x2),")")
  colnames(Diff.res) <- c("Variable","Value",xname,x1name,x2name,"Method","P","significance")
  return(Diff.res)
}

#function for extracting the importance in boruta
importance <- function(x){
  imp <- reshape2::melt(x$ImpHistory,na.rm = TRUE)[,-1]
  colnames(imp) <- c("Variable","Importance")
  imp <- imp[is.finite(imp$Importance),]
  variableGrp <- data.frame(Variable=names(x$finalDecision), 
                            finalDecision=x$finalDecision)
  showGrp <- data.frame(Variable=c("shadowMax", "shadowMean", "shadowMin"),
                        finalDecision=c("shadowMax", "shadowMean", "shadowMin"))
  variableGrp <- rbind(variableGrp, showGrp)
  boruta.variable.imp <- merge(imp, variableGrp, all.x=T)
  sortedVariable <- boruta.variable.imp %>% group_by(Variable) %>% 
    summarise(median=median(Importance)) %>% arrange(median)
  sortedVariable <- as.vector(sortedVariable$Variable)
  boruta.variable.imp$Variable <- factor(boruta.variable.imp$Variable, levels=sortedVariable)
  invisible(boruta.variable.imp)
}