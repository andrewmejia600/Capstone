### Create graphs and exploratory data analysis on improvement value of parcels

library(dplyr)
library(ggplot2)

df = read.csv("../../Data/df_all.csv")

# table for summary stats of improvement value of vacant parcels 
summary(df$impr_val[df$vac_par == 1])
# table for summary stats of improvement value of non-vacant parcels 
summary(df$impr_val[df$vac_par == 0])

# plot boxplots for log improvement value by vacant/nonvacant
df %>%
  ggplot(aes(x=factor(vac_par), y=log_impr_val, fill=factor(vac_par))) +
  geom_boxplot() +
  ggtitle("Log Improvement Value by Vacant/Non-Vacant") +
  xlab("Vacant (1) or Non-Vacant (0)")
