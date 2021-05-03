### Create graphs and exploratory data analysis on improvement value of parcels

library(dplyr)
library(ggplot2)

df = read.csv("../../Data/df_log_and_scaled.csv")

df %>%
  ggplot(aes(x=factor(vac_par), y=log_impr_val)) +
  geom_boxplot() 
