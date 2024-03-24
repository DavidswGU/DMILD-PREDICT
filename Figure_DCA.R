library(pROC)
library(dcurves)
source("Function.R")

#Combining dca data from models
models.dca.data <- cbind(xgb.test.dca,
                         as.data.frame(rf.test.dca),
                         as.data.frame(gbm.test.dca),
                         as.data.frame(knn.test.dca),
                         as.data.frame(gam.test.dca),
                         as.data.frame(ada.test.dca),
                         as.data.frame(svm.test.dca),
                         as.data.frame(lm.test.dca))
colnames(models.dca.data) <- c("Survival","XGBoost","RandomForest","GBM","KNN","General_additive","AdaBoost","SVM","LM")

models.dca <- dca(Survival~XGBoost+RandomForest+GBM+KNN+General_additive+AdaBoost+SVM+LM,data = models.dca.data,
                  as_probability = c("XGBoost","RandomForest","GBM","KNN","General_additive","AdaBoost","SVM","LM"))
models.dca <- standardized_net_benefit(models.dca)

#draw DCA curves
as_tibble(models.dca) %>%
  dplyr::filter(!is.na(standardized_net_benefit)) %>%
  ggplot(aes(x = threshold, y = standardized_net_benefit, color =label)) +
  stat_smooth(method = "loess", se = FALSE, formula = "y ~ x", 
              span = 0.2,linewidth=1) +
  coord_cartesian(ylim = c(-0.02, 1.02)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Standardized Net Benefit", 
       color = colorscheme) +
  scale_color_manual(values = c("#264653","#287271","#28827B","#2a9d8c","#8ab07d","#CBBD70","#e9c46b","#f3a261","#F1987A","#e66f71"))+
  theme_bw()