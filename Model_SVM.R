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
library(C50)
source("Function.R")
set.seed(58)

#tune parameters using grid search
data.recipe <- recipes::recipe(surv~.,data = train.data.select)%>%
  recipes::step_string2factor(all_nominal())%>%
  prep()
cv.recipe <- recipes::bake(data.recipe,new_data = train.data.select)%>%
  rsample::vfold_cv(v=10)
svm.spec <- parsnip::svm_linear(
  mode="classification",
  cost = tune()
)%>%
  set_engine("kernlab")
svm.param <- dials::parameters(
  cost()
  )
svm.param.grid <- dials::grid_max_entropy(
  svm.param,
  size = 60
)
svm.workflow <- workflows::workflow()%>%
  add_model(svm.spec)%>%
  add_formula(surv~.,)
svm.param.tune <- tune::tune_grid(
  object = svm.workflow,
  resamples = cv.recipe,
  grid = svm.param.grid,
  metrics = yardstick::metric_set(accuracy,f_meas),
  control = tune::control_grid(verbose=TRUE)
)
svm.param.tuned <- svm.param.tune%>%
  tune::select_best("accuracy")

#finalize model
svm.model.final <- svm.spec %>%
  finalize_model(svm.param.tuned)

#metrics in discovery cohort
train.prediction <- svm.model.final%>%parsnip::fit(formula = surv~.,
                                                   data = bake(data.recipe,new_data=train.data.select)
)%>%
  predict(new_data=bake(data.recipe,new_data=train.data.select),type="prob")%>%
  bind_cols(train.data.select[,ncol(train.data.select)])
train.prediction <- train.prediction%>%mutate(pred.class=ifelse(train.prediction$.pred_Deceased>=0.5,1,0))
train.prediction$...3 <- factor(train.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
svm.confmat.train <- confmat(as.factor(train.prediction$pred.class),train.prediction$...3)
auroc(train.prediction$.pred_Deceased,train.prediction$...3)

#metrics in validation cohort
test.prediction <- svm.model.final%>%parsnip::fit(formula = surv~.,
                                                  data = bake(data.recipe,new_data=train.data.select)
)%>%
  predict(new_data=bake(data.recipe,new_data=test.data.select),type="prob")%>%
  bind_cols(test.data.select[,ncol(test.data.select)])
test.prediction <- test.prediction%>%mutate(pred.class=ifelse(test.prediction$.pred_Deceased>=0.5,1,0))
test.prediction$...3 <- factor(test.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
svm.confmat.test <- confmat(as.factor(test.prediction$pred.class),test.prediction$...3)
auroc(test.prediction$.pred_Deceased,test.prediction$...3)

#data for DCA plot
svm.test.dca <- test.prediction$.pred_Deceased