library(kknn)
library(tidymodels)
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
knn.spec <- parsnip::nearest_neighbor(
  mode="classification",
  neighbors = tune()
)%>%
  set_engine("kknn",objective="binary")
knn.param <- dials::parameters(
  neighbors()
)
knn.param.grid <- dials::grid_max_entropy(
  knn.param,
  size = 60
)
knn.workflow <- workflows::workflow()%>%
  add_model(knn.spec)%>%
  add_formula(surv~.,)
knn.param.tune <- tune::tune_grid(
  object = knn.workflow,
  resamples = cv.recipe,
  grid = knn.param.grid,
  metrics = yardstick::metric_set(accuracy,f_meas),
  control = tune::control_grid(verbose=TRUE)
)
knn.param.tuned <- knn.param.tune%>%
  tune::select_best("accuracy")
#finalize model
knn.model.final <- knn.spec %>%
  finalize_model(knn.param.tuned)
saveRDS(knn.param.tuned,file = "knn.model.RDS")
#predictions in training set
train.prediction <- knn.model.final%>%parsnip::fit(formula = surv~.,
                                          data = bake(data.recipe,new_data=train.data.select)
)%>%
  predict(new_data=bake(data.recipe,new_data=train.data.select),type="prob")%>%
  bind_cols(train.data.select[,ncol(train.data.select)])
train.prediction <- train.prediction%>%mutate(pred.class=ifelse(train.prediction$.pred_Deceased>=0.5,1,0))
train.prediction$...3 <- factor(train.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
knn.confmat.train <- confmat(as.factor(train.prediction$pred.class),train.prediction$...3)
#predictions in test set
test.prediction <- knn.model.final%>%parsnip::fit(formula = surv~.,
                                         data = bake(data.recipe,new_data=train.data.select)
)%>%
  predict(new_data=bake(data.recipe,new_data=test.data.select),type="prob")%>%
  bind_cols(test.data.select[,ncol(test.data.select)])
test.prediction <- test.prediction%>%mutate(pred.class=ifelse(test.prediction$.pred_Deceased>=0.5,1,0))
test.prediction$...3 <- factor(test.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
knn.confmat.test <- confmat(as.factor(test.prediction$pred.class),test.prediction$...3)
#draw DCA plot
knn.test.dca <- test.prediction$.pred_Deceased
#use SHAP to explain model
##prepare data for SHAP
SHAP.data <- train.data[,c(vars.final,'surv')]
SHAP.data <- SHAP.data %>% mutate(No_DAD=ifelse(SHAP.data$DAD==0,1,0))
SHAP.data <- SHAP.data %>% mutate(No_NSIP=ifelse(SHAP.data$NSIP==0,1,0))
SHAP.data <- SHAP.data %>% mutate(MDA5=ifelse(SHAP.data$MDA5==0,1,0))
SHAP.engine <- knn.model.final%>%fit(formula = surv~.,
                                     data = bake(data.recipe,new_data=train.data.select))%>%
  extract_fit_engine()
SHAP.engine$feature_names <- colnames(SHAP.data[,!colnames(SHAP.data)%in%"surv"])
knn.SHAP <- shapviz(SHAP.engine,X_pred = as.matrix(SHAP.data[,!colnames(SHAP.data)%in%"surv"]),X= SHAP.data)
sv_importance(gbm.SHAP,kind="both",show_numbers = TRUE)
sv_dependence(gbm.SHAP,"NSIP",color_var = "auto")
sv_force(gbm.SHAP,row_id = 1)
sv_waterfall(gbm.SHAP,row_id = 1)
