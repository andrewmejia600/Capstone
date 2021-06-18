library(ggplot2)
if (!require(GGally)) install.packages('GGally')
library(GGally)
library(rlang)
library(purrr)
library(dplyr)
library(tidyr)
library(xgboost)
##########################################################
groupColors = c(NVAC="#325860", VAC="#FFA533")

kde_plots = function(df,.x_var, .y_var){
  
  p =  ggplot2::ggplot(data = df) +
    geom_density(aes(x = {{.x_var}}, 
                     group = {{.y_var}},
                     color = {{.y_var}}, 
                     fill = {{.y_var}}), 
                 alpha = .5, 
                 size = .5,
                 adjust = 4)
  
  p + ggtitle("Kernel Density of Classes XG-BOOST Model") +
    scale_fill_manual(values=groupColors) +
    theme_minimal() + theme(text = element_text(size=20), 
                            axis.title.x = element_text(size=20, face = "bold"),
                            axis.title.y = element_text(size=20, face = "bold")) + 
                            geom_hline(aes(yintercept = 0)) +
                            geom_vline(aes(xintercept = 0))
}                                                        #
#
##########################################################

read_data = read.csv('https://raw.githubusercontent.com/andrewmejia600/Capstone/main/Code/Models/XG_Model/XG_test_out.csv')

read_data['Class'] = factor(ifelse(read_data$VAC_PAR == 1, "VAC", "NVAC"))
read_data$num_division_cd = factor(read_data$num_division_cd)

data = read_data[,c(2:10)]




kde_plots(data,log_impr_val, Class)
kde_plots(data,num_division_cd, Class)
kde_plots(data,log_land_val, Class)
kde_plots(data,log_area_sqft, Class)
kde_plots(data,log_tot_val, Class)


######################################################################## Feature Importance 
githubURL <- ("https://raw.githubusercontent.com/andrewmejia600/Capstone/main/Code/Models/XG_Model/xgbFinal")
download.file(githubURL,"xgbFinal", method = "curl") 
XG_final =  xgb.load("xgbFinal")

simp_model = xgb.importance(feature_names = colnames(read_data[,c(3:8)]), model = XG_final)

simp_model_feats = data.frame(simp_model$Feature,simp_model$Frequency)
 + coord_flip()


p = ggplot(simp_model_feats, aes(x = reorder(simp_model.Feature,simp_model.Frequency), y =simp_model.Frequency )) + geom_bar(stat= "identity", width = 0.8) + geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0))
p + ggtitle("Feature Importance XG-BOOST Model") +theme_minimal() + labs(title = 'Feature Importance \n XG-BOOST', x = "", y = "")+ theme(text = element_text(size=22))  + coord_flip()


######################################################################## Full Model Feature Importance 
githubURL <- ("https://raw.githubusercontent.com/andrewmejia600/Capstone/main/Code/Models/XG_Model_Full/xgbFinal")
download.file(githubURL,"xgbFinal", method = "curl") 
XG_final =  xgb.load("xgbFinal")

read_data = read.csv('https://raw.githubusercontent.com/andrewmejia600/Capstone/main/Code/Models/XG_Model_Full/XG_test_out.csv')

full_model = xgb.importance(feature_names = colnames(read_data[,c(3:24)]), model = XG_final)

full_model_feats = data.frame(full_model$Feature,full_model$Frequency)

full_model_feats_s = full_model_feats[order(-full_model$Frequency),]
full_model_feats_s = full_model_feats_s[c(1:12),]

p = ggplot(full_model_feats_s, aes(x = reorder(full_model.Feature, full_model.Frequency), y = full_model.Frequency )) + geom_bar(stat= "identity", width = 0.8) + geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0))
p + ggtitle("Feature Importance XG-BOOST Model") +theme_minimal() + labs(title = 'Feature Importance \n XG-BOOST', x = "", y = "")+ theme(text = element_text(size=22))  + coord_flip()
