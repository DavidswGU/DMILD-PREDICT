library(glmnet)
library(tidymodels)
library(dplyr)
library(MLmetrics)
library(tune)
library(parsnip)
library(shapviz)
library(dcurves)
set.seed(58)
#tune parameters using grid search
data.recipe <- recipes::recipe(surv~.,data = train.data.select)%>%
  recipes::step_string2factor(all_nominal())%>%
  prep()
cv.recipe <- recipes::bake(data.recipe,new_data = train.data.select)%>%
  rsample::vfold_cv(v=10)
lasso.spec <- logistic_reg()%>%
  set_args(mixture=1,penalty=tune())%>%
  set_engine(engine = 'glmnet')%>%
  set_mode('classification')
lasso.param <- dials::parameters(
  penalty()
)
lasso.param.grid <- dials::grid_max_entropy(
  lasso.param,
  size = 60
)
lasso.workflow <- workflows::workflow()%>%
  add_model(lasso.spec)%>%
  add_formula(surv~.,)
lasso.param.tune <- tune::tune_grid(
  object = lasso.workflow,
  resamples = cv.recipe,
  grid = lasso.param.grid,
  metrics = yardstick::metric_set(accuracy,f_meas),
  control = tune::control_grid(verbose=TRUE)
)
lasso.param.tuned <- lasso.param.tune%>%
  tune::select_best("accuracy")
#finalize model
lasso.model.final <- lasso.spec %>%
  finalize_model(lasso.param.tuned)
saveRDS(lasso.model.final,file = "lasso.model.RDS")
#predictions in training set
train.prediction <- lasso.model.final%>%fit(formula = surv~.,
                                          data = bake(data.recipe,new_data=train.data.select)
)%>%
  predict(new_data=bake(data.recipe,new_data=train.data.select),type="prob")%>%
  bind_cols(train.data.select[,ncol(train.data.select)])
train.prediction <- train.prediction%>%mutate(pred.class=ifelse(train.prediction$.pred_Deceased>=0.5,1,0))
train.prediction$...3 <- factor(train.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
lasso.confmat.train <- confmat(as.factor(train.prediction$pred.class),train.prediction$...3)
#predictions in test set
test.prediction <- lasso.model.final%>%parsnip::fit(formula = surv~.,
                                         data = bake(data.recipe,new_data=train.data.select)
)%>%
  predict(new_data=bake(data.recipe,new_data=test.data.select),type="prob")%>%
  bind_cols(test.data.select[,ncol(test.data.select)])
test.prediction <- test.prediction%>%mutate(pred.class=ifelse(test.prediction$.pred_Deceased>=0.5,1,0))
test.prediction$...3 <- factor(test.prediction$...3,levels = c("Deceased","Survived"),labels = c(1,0))
lasso.confmat.test <- confmat(as.factor(test.prediction$pred.class),test.prediction$...3)
#draw DCA plot
lasso.test.dca <- test.prediction$.pred_Deceased
#use SHAP to explain model
##prepare data for SHAP
SHAP.data <- train.data[,c(vars.final,'surv')]
SHAP.data <- SHAP.data %>% mutate(No_DAD=ifelse(SHAP.data$DAD==0,1,0))
SHAP.data <- SHAP.data %>% mutate(No_NSIP=ifelse(SHAP.data$NSIP==0,1,0))
SHAP.data <- SHAP.data %>% mutate(MDA5=ifelse(SHAP.data$MDA5==0,1,0))
SHAP.engine <- lasso.model.final%>%fit(formula = surv~.,
                                     data = bake(data.recipe,new_data=train.data.select))%>%
  extract_fit_engine()
SHAP.engine$feature_names <- colnames(SHAP.data[,!colnames(SHAP.data)%in%"surv"])
lasso.SHAP <- shapviz(SHAP.engine,X_pred = as.matrix(SHAP.data[,!colnames(SHAP.data)%in%"surv"]),X= SHAP.data)
sv_importance(lasso.SHAP,kind="both",show_numbers = TRUE)
sv_dependence(lasso.SHAP,"NSIP",color_var = "auto")
sv_force(lasso.SHAP,row_id = 1)
sv_waterfall(lasso.SHAP,row_id = 1)
