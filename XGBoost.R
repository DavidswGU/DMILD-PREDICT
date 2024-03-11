library(xgboost)
library(tidymodels)
library(caret)
library(dplyr)
library(MLmetrics)
library(tune)
library(parsnip)
library(dcurves)
library(shapr)
set.seed(58)
#tune parameters
data.recipe <- recipes::recipe(surv~.,data = train.data.select)%>%
  recipes::step_string2factor(all_nominal())%>%
  prep()
cv.recipe <- recipes::bake(data.recipe,new_data = train.data.select)%>%
  rsample::vfold_cv(v=10)
xgb.spec <- parsnip::boost_tree(
  mode="classification",
  trees=1000,
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
)%>%
  set_engine("xgboost",objective="reg:squarederror")
xgb.param <- dials::parameters(
  min_n(),
  tree_depth(),
  learn_rate(),
  loss_reduction()
)
xgb.param.grid <- dials::grid_max_entropy(
  xgb.param,
  size = 60
)
xgb.workflow <- workflows::workflow()%>%
  add_model(xgb.spec)%>%
  add_formula(surv~.,)
xgb.param.tune <- tune::tune_grid(
  object = xgb.workflow,
  resamples = cv.recipe,
  grid = xgb.param.grid,
  metrics = yardstick::metric_set(accuracy,f_meas),
  control = tune::control_grid(verbose=TRUE)
)
xgb.param.tuned <- xgb.param.tune%>%
  tune::select_best("accuracy")
#finalize model
xgb.model.final <- xgb.spec %>%
  finalize_model(xgb.param.tuned)
saveRDS(xgb.param.tuned,file = "xgb.model.RDS")
#predictions in training set
train.prediction <- xgb.model.final%>%fit(formula = surv~.,
                                          data = bake(data.recipe,new_data=train.data.select)
                                          )%>%
  predict(new_data=bake(data.recipe,new_data=train.data.select),type="prob")%>%
  bind_cols(train.data.select[,ncol(train.data.select)])
train.prediction <- train.prediction%>%mutate(pred.class=ifelse(train.prediction$.pred_Deceased>=0.5,1,0))
train.prediction$...3 <- factor(train.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
xgb.confmat.train <- confmat(as.factor(train.prediction$pred.class),train.prediction$...3)
#predictions in test set
test.prediction <- xgb.model.final%>%parsnip::fit(formula = surv~.,
                                         data = bake(data.recipe,new_data=train.data.select)
)%>%
  predict(new_data=bake(data.recipe,new_data=test.data.select),type="prob")%>%
  bind_cols(test.data.select[,ncol(test.data.select)])
test.prediction <- test.prediction%>%mutate(pred.class=ifelse(test.prediction$.pred_Deceased>=0.5,1,0))
test.prediction$...3 <- factor(test.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
xgb.confmat.test <- confmat(as.factor(test.prediction$pred.class),test.prediction$...3)
#draw DCA plot
xgb.test.dca <- test.prediction
xgb.test.dca$Survival <- test.data$surv
xgb.test.dca$XGBoost <- xgb.test.dca$.pred_Deceased
xgb.test.dca <- xgb.test.dca[,c("Survival","XGBoost")]
xgb.dca <- dca(Survival~XGBoost, data = xgb.test.dca,as_probability = "XGBoost")
plot(xgb.dca,smooth = TRUE)
#use SHAP to explain model
predict_model <- function(model,newdata){
  test.prediction <-predict(model,as.data.frame(newdata),type="prob")
  test.prediction <- test.prediction$.pred_Deceased
}
xgb.fit <- xgb.model.final%>%fit(formula = surv~.,
                                 data = bake(data.recipe,new_data=train.data.select)
)
train.data.baked <- bake(data.recipe,new_data=train.data.select)
test.data.baked <- bake(data.recipe,new_data=test.data.select)
p0=mean(test.data$surv)
explanation.xgb <- explain(
  model=xgb.fit,
  x_explain=as.data.frame(test.data.baked[5,-14]),
  x_train=train.data.baked[,-14],
  approach="ctree",
  prediction_zero=p0,
  predict_model=predict_model,
  n_batches=1
)
plot(explanation.xgb, bar_plot_phi0 = FALSE,col = c("pink","lightgreen"),pch=20)
