### Create graphs and exploratory data analysis on zoning

library(dplyr)
library(ggplot2)

df = read.csv("../../Data/df_all.csv")

# plot boxplots for log improvement value by vacant/nonvacant
df %>%
  ggplot(aes(x=zoning_buckets, fill=factor(vac_par))) +
  geom_bar(position="fill") +
  ggtitle("Zoning by Vacant/Non-Vacant") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave(
  filename = "zoning_proportion_barchart.png",
  plot = last_plot(),
  path = "../../Figures/")
