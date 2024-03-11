library(Boruta)
library(randomForest)
library(pROC)
library(dplyr)
library(ImageGP)
library(WVPlots)
library(dplyr)
library(ggcorrplot)
set.seed(58)
DM <- read.csv(file = "DM_ILD_orig.csv")
DM.data <- DM[,-1]
DM.data <- DM.data[,-grep("survtime",colnames(DM.data))]
train.data <- DM.data[1:201,]
train.pred <- train.data[,-grep("surv",colnames(train.data))]
train.target <- as.factor(train.data$surv)
test.data <- DM.data[202:253,]
boruta <- Boruta(x=train.pred,y=train.target,pValue=0.01,mcAdj=TRUE,maxRuns=300)
table(boruta$finalDecision)
boruta$finalDecision[boruta$finalDecision=="Confirmed"]
plotImpHistory(boruta)
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
boruta.variable.imp <- importance(boruta)
#draw the boxplot for final decision
sp_boxplot(boruta.variable.imp, melted=T, xvariable = "Variable", yvariable = "Importance",
           legend_variable = "finalDecision", legend_variable_order = c("shadowMax", "shadowMean", "shadowMin", "Confirmed"),
           xtics_angle = 90)
boruta.finalVars <- data.frame(Item=getSelectedAttributes(boruta, withTentative = F), Type="Boruta")
train.data.select <- train.data[ ,c(boruta.finalVars$Item, 'surv')]
test.data.select <- test.data[ ,c(boruta.finalVars$Item, 'surv')]
train.data.select <- train.data.select %>% mutate(DAD=ifelse(train.data.select$DAD==1,"Yes","No"))
train.data.select <- train.data.select %>% mutate(NSIP=ifelse(train.data.select$NSIP==1,"Yes","No"))
train.data.select <- train.data.select %>% mutate(RPILD=ifelse(train.data.select$RPILD==1,"Yes","No"))
train.data.select <- train.data.select %>% mutate(surv=ifelse(train.data.select$surv==1,"Deceased","Survived"))
train.data.select <- train.data.select %>% mutate(MDA5=ifelse(train.data.select$MDA5==1,"Yes","No"))
test.data.select <- test.data.select %>% mutate(DAD=ifelse(test.data.select$DAD==1,"Yes","No"))
test.data.select <- test.data.select %>% mutate(NSIP=ifelse(test.data.select$NSIP==1,"Yes","No"))
test.data.select <- test.data.select %>% mutate(RPILD=ifelse(test.data.select$RPILD==1,"Yes","No"))
test.data.select <- test.data.select %>% mutate(MDA5=ifelse(test.data.select$MDA5==1,"Yes","No"))
test.data.select <- test.data.select %>% mutate(surv=ifelse(test.data.select$surv==1,"Deceased","Survived"))
#correlation plots
PairPlot(train.data.select,colnames(train.data.select)[4:12],
         "Scatter plot matrix for selected predictors",group_var ="surv" )
corr <- round(cor(train.data.select),3)
corr.mtx <- cor_pmat(train.data.select)
ggcorrplot(corr,hc.order = TRUE,
           ggtheme = ggplot2::theme_classic(base_size = 15),
           colors = c("violet","white","lightblue"),
           lab=TRUE,lab_size = 3,
           tl.cex = 13,
           p.mat = corr.mtx,
           sig.level = 0.001,
           pch = 4,
           pch.cex = 10,
           insig = "pch")
