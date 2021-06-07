#Model exploration on RF_MII_Model.r
library(randomForestExplainer)

setwd('/Users/purvisfam/OneDrive/Documents/Masters/CAPSTONE')
data = read.csv('RF_test_out.csv')
#read in RDS file
random_forest_final = readRDS('RF_final_model.rds', refhook = NULL)

#join the data on the original prediction, smaller dataset only.  Join on X
#df2 = read.csv('df_log_and_scaled.csv')
data = read.csv('df_log_and_scaled.csv')
#df = merge(data,df2$vac_par, by.x=1, by.y=0)

plot(density(df$VAC_PAR)) #target

# plot(density(df$Tuned_RF_preds_Cut), main="Kernel Density of Prediction", 
#      xlab = "Vacant Parcel",
#      ylab = "Density")
# 
# plot(density(df$VAC_PAR), main="Kernel Density of Prediction", 
#      xlab = "Vacant Parcel",
#      ylab = "Density")

getTree(random_forest_final, labelVar = TRUE)

randomForestExplainer::explain_forest(random_forest_final, interactions = TRUE, data=data)
randomForestExplainer::plot_importance_ggpairs(random_forest_final)
randomForestExplainer::plot_min_depth_interactions(random_forest_final)
randomForestExplainer::plot_min_depth_distribution(random_forest_final)
randomForestExplainer::plot_importance_rankings(random_forest_final)
randomForestExplainer::plot_multi_way_importance(random_forest_final)

