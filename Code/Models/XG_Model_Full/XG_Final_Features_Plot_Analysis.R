library(ggplot2)
if (!require(GGally)) install.packages('GGally')
library(GGally)
library(rlang)
library(purrr)
library(dplyr)
library(tidyr)

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
  
  p + ggtitle("Kernel Density of Classes Based on Feature XG-BOOST Model") +
    scale_fill_manual(values=groupColors) +
    theme_minimal()
}                                                        #
#
##########################################################

read_data = read.csv('https://raw.githubusercontent.com/andrewmejia600/Capstone/main/Code/Models/XG_Model_Full/XG_test_out.csv')

read_data['Class'] = factor(ifelse(read_data$VAC_PAR == 1, "VAC", "NVAC"))
read_data$num_division_cd = factor(read_data$num_division_cd)

data = read_data[,c(2:25)]




kde_plots(data,log_impr_val, Class)
kde_plots(data,num_division_cd, Class)
kde_plots(data,log_land_val, Class)
kde_plots(data,log_area_sqft, Class)
kde_plots(data,log_tot_val, Class)

kde_plots(data, zoning_buckets,Class)
kde_plots(data, num_sptd, Class)
kde_plots(data, permit_type, Class)
kde_plots(data, num_nbhd_cd, Class)
kde_plots(data, days_since_permit_scaled, Class)





