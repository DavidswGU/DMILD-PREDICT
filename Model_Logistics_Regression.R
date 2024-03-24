library(tidymodels)
library(caret)
library(dplyr)
library(MLmetrics)
library(tune)
library(parsnip)
library(shapviz)
library(dcurves)
library(bonsai)
library(shapr)
source("Function.R")
set.seed(58)

#fit logistics regression model
data.recipe <- recipes::recipe(surv~.,data = train.data.select)%>%
  recipes::step_string2factor(all_nominal())%>%
  prep()
lm.spec <- parsnip::logistic_reg()%>%
  set_engine("glm")
lm.workflow <- workflows::workflow()%>%
  add_model(lm.spec)%>%
  add_formula(surv~.,)

#metrics in discovery cohort
train.prediction <- lm.workflow%>%parsnip::fit(data = bake(data.recipe,new_data=train.data.select)
)%>%
  predict(new_data=bake(data.recipe,new_data=train.data.select),type="prob")%>%
  bind_cols(train.data.select[,ncol(train.data.select)])
train.prediction <- train.prediction%>%mutate(pred.class=ifelse(train.prediction$.pred_Deceased>=0.5,1,0))
train.prediction$...3 <- factor(train.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
lm.confmat.train <- confmat(as.factor(train.prediction$pred.class),train.prediction$...3)
auroc(train.prediction$.pred_Deceased,train.prediction$...3)

#metrics in validation cohort
test.prediction <- lm.workflow%>%parsnip::fit(data = bake(data.recipe,new_data=train.data.select)
)%>%
  predict(new_data=bake(data.recipe,new_data=test.data.select),type="prob")%>%
  bind_cols(test.data.select[,ncol(test.data.select)])
test.prediction <- test.prediction%>%mutate(pred.class=ifelse(test.prediction$.pred_Deceased>=0.5,1,0))
test.prediction$...3 <- factor(test.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
lm.confmat.test <- confmat(as.factor(test.prediction$pred.class),test.prediction$...3)
auroc(test.prediction$.pred_Deceased,test.prediction$...3)

#data for DCA plot
lm.test.dca <- test.prediction$.pred_Deceased