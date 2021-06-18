library(ggplot2)
if (!require(GGally)) install.packages('GGally')
library(GGally)
library(rlang)
library(purrr)
library(dplyr)
library(tidyr)

########################################################################### Full Data 

##########################################################
groupColors = c(NVAC="#325860", VAC="#FFA533")

kde_plots = function(df,.x_var, .y_var){
  
  p =  ggplot2::ggplot(data = df) +
    geom_density(aes(x = {{.x_var}}, 
                     group = {{.y_var}},
                     color = {{.y_var}}, 
                     fill = {{.y_var}}), 
                 alpha = .5, 
                 size = .9,
                 adjust =20)
  
  p + ggtitle("Kernel Density of Classes") +
    scale_fill_manual(values=groupColors) +
    theme_minimal() + theme(text = element_text(size=20), 
                            axis.title.x = element_text(size=20, face = "bold"),
                            axis.title.y = element_text(size=20, face = "bold")) + 
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = 0))
}                                                        #
#
##########################################################

read_data = read.csv('https://raw.githubusercontent.com/andrewmejia600/Capstone/main/Data/df_log_and_scaled.csv')

read_data_not_log = read.csv('https://raw.githubusercontent.com/andrewmejia600/Capstone/main/Data/df_not_scaled.csv')



read_data['Class'] = factor(ifelse(read_data$vac_par == 1, "VAC", "NVAC"))

read_data$num_division_cd = factor(read_data$num_division_cd)

read_data_not_log['Class'] = factor(ifelse(read_data_not_log$vac_par == 1, "VAC", "NVAC"))

read_data_not_log$num_division_cd = factor(read_data_not_log$num_division_cd)

read_data_not_log_filt_crime = read_data_not_log %>% filter(count_of_crime <= 25)

read_data_not_log_filt_311 = read_data_not_log %>% filter(count_of_311 <= 5)

read_data_scaled_filt_crime = read_data %>% filter(count_of_crime_scaled <= 7)

read_data_scaled_filt_311 = read_data %>% filter(count_of_311_scaled <= 7)

kde_plots(read_data,num_sptd, Class)
kde_plots(read_data,log_impr_val, Class)
kde_plots(read_data,log_tot_val, Class)
kde_plots(read_data,num_division_cd, Class)
kde_plots(read_data,log_land_val, Class)
kde_plots(read_data, log_area_sqft, Class)
kde_plots(read_data, zoning_buckets, Class)
kde_plots(read_data, CO_sqft_scaled, Class)
kde_plots(read_data, occupancy, Class)
kde_plots(read_data, count_of_311_scaled, Class)
kde_plots(read_data, count_of_crime_scaled, Class)


kde_plots(read_data_not_log_filt_crime, count_of_crime, Class)

kde_plots(read_data_not_log_filt_311,count_of_311, Class)

kde_plots(read_data_scaled_filt_crime, count_of_crime_scaled, Class)

kde_plots(read_data_scaled_filt_311,count_of_311_scaled, Class)








