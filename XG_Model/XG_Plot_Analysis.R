library(ggplot2)
if (!require(GGally)) install.packages('GGally')
library(GGally)
library(rlang)
library(purrr)
library(dplyr)
library(tidyr)

##########################################################
kde_plots = function(df,.x_var, .y_var){
  
  ggplot2::ggplot(data = df) +
    geom_density(aes(x = {{.x_var}}, 
                     group = {{.y_var}},
                     color = {{.y_var}}, 
                     fill = {{.y_var}}), 
                 alpha = .2, 
                 size = .25) +
    theme_minimal()
}                                                        #
                                                         #
##########################################################

read_data = read.csv('XG_test_out.csv')

data = read_data[,c(2:9)]

summary(data[,c(2:8)])
#ggpairs(data,ggplot2::aes(colour=as.factor(VAC_PAR)))

ggplot2::ggplot(data = data, aes(x = as.factor(VAC_PAR), y = BaseLine_RF_Preds, fill =as.factor(VAC_PAR) )) + geom_boxplot() 
ggplot2::ggplot(data = data, aes(x = as.factor(VAC_PAR), y = Tuned_RF_Preds, fill =as.factor(VAC_PAR) )) + geom_boxplot() 





kde_plots(data,num_division_cd, VAC_PAR)
kde_plots(data,log_impr_val, VAC_PAR)
kde_plots(data, log_tot_val, VAC_PAR)
kde_plots(data, log_land_val, VAC_PAR)
kde_plots(data,log_area_sqft, VAC_PAR)

kde_plots(data,num_division_cd, Tuned_XG_preds_Cut)
kde_plots(data,log_impr_val, Tuned_XG_preds_Cut)
kde_plots(data, log_tot_val, Tuned_XG_preds_Cut)
kde_plots(data, log_land_val, Tuned_XG_preds_Cut)
kde_plots(data,log_area_sqft, Tuned_XG_preds_Cut)

model_misses = data %>% filter(VAC_PAR != Tuned_XG_preds_Cut)
model_correct = data %>% filter(VAC_PAR == Tuned_XG_preds_Cut)

kde_plots(model_misses, num_division_cd, Tuned_XG_preds_Cut)
kde_plots(model_misses, num_division_cd, VAC_PAR)

kde_plots(model_correct, num_division_cd, Tuned_XG_preds_Cut)
kde_plots(model_correct, num_division_cd, VAC_PAR)

kde_plots(model_misses, log_impr_val, Tuned_XG_preds_Cut)

kde_plots(model_correct, log_impr_val, Tuned_XG_preds_Cut)


kde_plots(model_misses, log_impr_val, Tuned_XG_preds_Cut)


kde_plots(model_correct, log_impr_val, Tuned_XG_preds_Cut)



