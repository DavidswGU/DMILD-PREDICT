library(randomForest)
library(tidymodels)
library(caret)
library(dplyr)
library(MLmetrics)
library(tune)
library(parsnip)
library(shapviz)
library(dcurves)
library(bonsai)
set.seed(58)
#tune parameters using grid search
data.recipe <- recipes::recipe(surv~.,data = train.data.select)%>%
  recipes::step_string2factor(all_nominal())%>%
  prep()
cv.recipe <- recipes::bake(data.recipe,new_data = train.data.select)%>%
  rsample::vfold_cv(v=10)
rf.spec <- parsnip::rand_forest(
  mode="classification",
  trees=tune(),
  min_n = tune(),
  mtry = 2
)%>%
  set_engine("ranger")
rf.param <- dials::parameters(
  min_n(),
  trees()
)
rf.param.grid <- dials::grid_max_entropy(
  rf.param,
  size = 60
)
rf.workflow <- workflows::workflow()%>%
  add_model(rf.spec)%>%
  add_formula(surv~.,)
rf.param.tune <- tune::tune_grid(
  object = rf.workflow,
  resamples = cv.recipe,
  grid = rf.param.grid,
  metrics = yardstick::metric_set(accuracy,f_meas),
  control = tune::control_grid(verbose=TRUE)
)
rf.param.tuned <- rf.param.tune%>%
  tune::select_best("accuracy")
#finalize model
rf.model.final <- rf.spec %>%
  finalize_model(rf.param.tuned)
save(rf.model.final,file = "rf.model.RData")
#predictions in training set
train.prediction <- rf.model.final%>%parsnip::fit(formula = surv~.,
                                          data = bake(data.recipe,new_data=train.data.select)
)%>%
  predict(new_data=bake(data.recipe,new_data=train.data.select),type="prob")%>%
  bind_cols(train.data.select[,ncol(train.data.select)])
train.prediction <- train.prediction%>%mutate(pred.class=ifelse(train.prediction$.pred_Deceased>=0.5,1,0))
train.prediction$...3 <- factor(train.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
rf.confmat.train <- confmat(as.factor(train.prediction$pred.class),train.prediction$...3)
#predictions in test set
test.prediction <- rf.model.final%>%parsnip::fit(formula = surv~.,
                                         data = bake(data.recipe,new_data=train.data.select)
)%>%
  predict(new_data=bake(data.recipe,new_data=test.data.select),type="prob")%>%
  bind_cols(test.data.select[,ncol(test.data.select)])
test.prediction <- test.prediction%>%mutate(pred.class=ifelse(test.prediction$.pred_Deceased>=0.5,1,0))
test.prediction$...3 <- factor(test.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
rf.confmat.test <- confmat(as.factor(test.prediction$pred.class),test.prediction$...3)
#draw DCA plot
rf.test.dca <- test.prediction$.pred_Deceased
#use SHAP to explain model
##prepare data for SHAP
#use SHAP to explain model
##prepare data for SHAP
predict_model <- function(model,newdata){
  test.prediction <-predict(model,as.data.frame(newdata),type="prob")
  test.prediction <- test.prediction$.pred_Deceased
}
rf.fit <- rf.model.final%>%fit(formula = surv~.,
                                 data = bake(data.recipe,new_data=train.data.select)
)
train.data.baked <- bake(data.recipe,new_data=train.data.select)
test.data.baked <- bake(data.recipe,new_data=test.data.select)
p0=mean(test.data$surv)
explanation.rf <- explain(
  model=rf.fit,
  x_explain=test.data.baked[5,-14],
  x_train=train.data.baked[,-14],
  approach="ctree",
  prediction_zero=p0,
  predict_model=predict_model,
  n_batches=1
)
plot(explanation.rf, bar_plot_phi0 = FALSE,col = c("#e66f51","#8ab07d"),pch=20)
