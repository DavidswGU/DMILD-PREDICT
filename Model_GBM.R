library(gbm)
library(lightgbm)
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

#tune parameters using grid search
data.recipe <- recipes::recipe(surv~.,data = train.data.select)%>%
  recipes::step_string2factor(all_nominal())%>%
  prep()
cv.recipe <- recipes::bake(data.recipe,new_data = train.data.select)%>%
  rsample::vfold_cv(v=10)
gbm.spec <- parsnip::boost_tree(
  mode="classification",
  trees=1000,
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
)%>%
  set_engine("lightgbm",objective="binary")
gbm.param <- dials::parameters(
  min_n(),
  tree_depth(),
  learn_rate(),
  loss_reduction()
)
gbm.param.grid <- dials::grid_max_entropy(
  gbm.param,
  size = 60
)
gbm.workflow <- workflows::workflow()%>%
  add_model(gbm.spec)%>%
  add_formula(surv~.,)
gbm.param.tune <- tune::tune_grid(
  object = gbm.workflow,
  resamples = cv.recipe,
  grid = gbm.param.grid,
  metrics = yardstick::metric_set(accuracy,f_meas),
  control = tune::control_grid(verbose=TRUE)
)
  gbm.param.tuned <- gbm.param.tune%>%
  tune::select_best("accuracy")
  
#finalize model
gbm.model.final <- gbm.spec %>%
  finalize_model(gbm.param.tuned)

#metrics in training set
train.prediction <- gbm.model.final%>%fit(formula = surv~.,
                                          data = bake(data.recipe,new_data=train.data.select)
)%>%
  predict(new_data=bake(data.recipe,new_data=train.data.select),type="prob")%>%
  bind_cols(train.data.select[,ncol(train.data.select)])
train.prediction <- train.prediction%>%mutate(pred.class=ifelse(train.prediction$.pred_Deceased>=0.5,1,0))
train.prediction$...3 <- factor(train.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
gbm.confmat.train <- confmat(as.factor(train.prediction$pred.class),train.prediction$...3)
auroc(train.prediction$.pred_Deceased,train.prediction$...3)

#metrics in test set
test.prediction <- gbm.model.final%>%parsnip::fit(formula = surv~.,
                                         data = bake(data.recipe,new_data=train.data.select)
)%>%
  predict(new_data=bake(data.recipe,new_data=test.data.select),type="prob")%>%
  bind_cols(test.data.select[,ncol(test.data.select)])
test.prediction <- test.prediction%>%mutate(pred.class=ifelse(test.prediction$.pred_Deceased>=0.5,1,0))
test.prediction$...3 <- factor(test.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
auroc(test.prediction$.pred_Deceased,test.prediction$...3)
gbm.confmat.test <- confmat(as.factor(test.prediction$pred.class),test.prediction$...3)

#prepare data for DCA
gbm.test.dca <- test.prediction$.pred_Deceased