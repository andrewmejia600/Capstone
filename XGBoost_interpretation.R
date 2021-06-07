#https://liuyanguu.github.io/post/2019/07/18/visualization-of-shap-for-xgboost/
  
install.packages("SHAPforxgboost")

library(SHAPforxgboost)
library(data.table)
library(here)
library(ggplot2)
library(xgboost)

xgb_model = readRDS('XG_final_model.rds', refhook = NULL)

#need to have loaded dtrain from the model test/train split to get the comparisons
shap_values <- shap.values(xgb_model = xgboost_best, X_train = dtrain)
shap_values$mean_shap_score

shap_data <- copy(shap_values$shap_score)
shap_data[, BIAS := shap_values$BIAS0]
pred_mod <- predict(xgboost_best, dtrain, ntreelimit = 10)
shap_data[, `:=`(rowSum = round(rowSums(shap_data),6), pred_mod = round(pred_mod,6))]
rmarkdown::paged_table(shap_data[1:20,])

install.packages("devtools")
library(devtools)
install_github("AppliedDataSciencePartners/xgboostExplainer")

library(xgboostExplainer)
explainer = buildExplainer(xgboost_best,dtrain, type="binary", base_score = 0.5, trees_idx = NULL)
pred.breakdown = explainPredictions(xgboost_best, explainer, dtest)

#breaks down specific observations
showWaterfall(xgboost_best, explainer, dtest, test, 2, type = "binary")
showWaterfall(xgboost_best, explainer, dtest, test, 8, type = "binary")
showWaterfall(xgboost_best, explainer, dtest, test, 238, type = "binary")

# plot the zero tree
library(DiagrammeR)
xgb.plot.tree(model = xgboost_best, trees = 0)
