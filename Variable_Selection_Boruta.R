library(Boruta)
library(randomForest)
library(dplyr)
library(ImageGP)
library(WVPlots)
library(dplyr)
source("Function.R")

set.seed(58)
#divide the cohort
DM <- read.csv(file = "DM_ILD_orig.csv")
DM.data <- DM[,-1]
DM.data <- DM.data[,-grep("survtime",colnames(DM.data))]
train.data <- DM.data[1:201,]
train.pred <- train.data[,-grep("surv",colnames(train.data))]
train.target <- as.factor(train.data$surv)
test.data <- DM.data[202:253,]

#variable selection with boruta
boruta <- Boruta(x=train.pred,y=train.target,pValue=0.01,mcAdj=TRUE,maxRuns=300)
table(boruta$finalDecision)
boruta$finalDecision[boruta$finalDecision=="Confirmed"]
plotImpHistory(boruta)
boruta.variable.imp <- importance(boruta)

#draw the boxplot for final decision
sp_boxplot(boruta.variable.imp, melted=T, xvariable = "Variable", yvariable = "Importance",
           legend_variable = "finalDecision", legend_variable_order = c("shadowMax", "shadowMean", "shadowMin", "Confirmed"),
           xtics_angle = 90)

#select and convert data for modeling
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
