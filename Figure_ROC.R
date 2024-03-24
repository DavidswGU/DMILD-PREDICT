library(fbroc)
set.seed(58)

#plot roc curves for top 4 models with highest AUROC
rf.roc <- plotroc(models.dca.data$RandomForest,true.class,color = c("#2a9d8c","#287271"))
ada.roc <- plotroc(models.dca.data$AdaBoost,true.class,color = c("#8ab09d","#8ab05d"))
svm.roc <- plotroc(models.dca.data$SVM,true.class,color = c("#f3a281","#f3a241"))
gbm.roc <- plotroc(models.dca.data$GBM,true.class,color = c("#e66f50","#e66f71"))
