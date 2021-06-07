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

read_data = read.csv('RF_test_out.csv')

data = read_data[,c(2:12)]

summary(data[,c(8:11)])
#ggpairs(data,ggplot2::aes(colour=as.factor(VAC_PAR)))

ggplot2::ggplot(data = data, aes(x = as.factor(VAC_PAR), y = BaseLine_RF_Preds, fill =as.factor(VAC_PAR) )) + geom_boxplot() 
ggplot2::ggplot(data = data, aes(x = as.factor(VAC_PAR), y = Tuned_RF_Preds, fill =as.factor(VAC_PAR) )) + geom_boxplot() 




kde_plots(data, num_sptd, VAC_PAR)
kde_plots(data,num_division_cd, VAC_PAR)
kde_plots(data, zoning_buckets, VAC_PAR)
kde_plots(data,log_impr_val, VAC_PAR)
kde_plots(data, log_tot_val, VAC_PAR)
kde_plots(data, permit_type, VAC_PAR)

kde_plots(data, num_sptd, Tuned_RF_Preds)
kde_plots(data,num_division_cd, Tuned_RF_Preds)
kde_plots(data, zoning_buckets, Tuned_RF_Preds)
kde_plots(data,log_impr_val, Tuned_RF_Preds)
kde_plots(data, log_tot_val, Tuned_RF_Preds)
kde_plots(data, permit_type, Tuned_RF_Preds)

model_misses = data %>% filter(VAC_PAR != Tuned_RF_preds_Cut)
model_correct = data %>% filter(VAC_PAR == Tuned_RF_preds_Cut)

kde_plots(model_misses, num_sptd, Tuned_RF_preds_Cut)
kde_plots(model_misses, num_sptd, VAC_PAR)

kde_plots(model_correct, num_sptd, Tuned_RF_preds_Cut)
kde_plots(model_correct, num_sptd, VAC_PAR)

kde_plots(model_misses, zoning_buckets, Tuned_RF_preds_Cut)

kde_plots(model_correct, zoning_buckets, Tuned_RF_preds_Cut)


kde_plots(model_misses, log_impr_val, Tuned_RF_preds_Cut)


kde_plots(model_correct, log_impr_val, Tuned_RF_preds_Cut)

test = data %>% filter(num_sptd == 2)

test_2 = model_misses %>% filter(num_sptd == 2)

test %>% group_by(VAC_PAR, num_sptd) %>% summarise(n = n())
data %>% group_by(VAC_PAR, num_sptd) %>% summarise(n = n())

test_2 %>% group_by(VAC_PAR, num_sptd) %>% summarise(n = n())
