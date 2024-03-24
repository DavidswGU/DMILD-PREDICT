library(mgcv)
library(tidymodels)
library(dplyr)
library(MLmetrics)
library(tune)
library(parsnip)
library(shapr)
library(party)
source("Function.R")
set.seed(58)

#set GAM
data.recipe <- recipes::recipe(surv~.,data = train.data.select)%>%
  recipes::step_string2factor(all_nominal())%>%
  prep()
gam.spec <- gen_additive_mod()%>%
  set_args(select_features=FALSE,adjust_deg_free=14)%>%
  set_engine(engine = 'mgcv')%>%
  set_mode('classification')

#metrics in discovery cohort
train.prediction <- gam.spec%>%parsnip::fit(formula = surv~NSIP+DAD+MDA5+RPILD+NE_pct+LY+N_L+PCT+ALB+CD3+CD4+CD8+PO2,
               data = bake(data.recipe,new_data=train.data.select)
)%>%
  predict(new_data=bake(data.recipe,new_data=train.data.select),type="prob")%>%
  bind_cols(train.data.select[,ncol(train.data.select)])
train.prediction <- train.prediction%>%mutate(pred.class=ifelse(train.prediction$.pred_Deceased>=0.5,1,0))
train.prediction$...3 <- factor(train.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
gam.confmat.train <- confmat(as.factor(train.prediction$pred.class),train.prediction$...3)

#metrics in validation cohort
test.prediction <- gam.spec%>%parsnip::fit(formula = surv~NSIP+DAD+MDA5+RPILD+NE_pct+LY+N_L+PCT+ALB+CD3+CD4+CD8+PO2,
                                           data = bake(data.recipe,new_data=train.data.select)
)%>%
  predict(new_data=bake(data.recipe,new_data=test.data.select),type="prob")%>%
  bind_cols(test.data.select[,ncol(test.data.select)])
test.prediction <- test.prediction%>%mutate(pred.class=ifelse(test.prediction$.pred_Deceased>=0.5,1,0))
test.prediction$...3 <- factor(test.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
gam.confmat.test <- confmat(as.factor(test.prediction$pred.class),test.prediction$...3)

#draw DCA plot
gam.test.dca <- test.prediction$.pred_Deceased