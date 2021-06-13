### Create graphs and exploratory data analysis on improvement value of parcels

library(dplyr)
library(ggplot2)

df = read.csv("../../Data/df_all.csv")

# table for summary stats of improvement value of vacant parcels 
summary(df$impr_val[df$vac_par == 1])
# table for summary stats of improvement value of non-vacant parcels 
summary(df$impr_val[df$vac_par == 0])

# scatterplots for improvement value, division code
df %>%
  sample_n(1000) %>%
  ggplot(aes(x=factor(num_division_cd), y=log_impr_val, color=factor(vac_par))) +
  geom_point(position = "jitter", aes(alpha=.5)) +
  ggtitle("Log Improvement Value by Division Code by Vacant/Non-Vacant") +
  xlab("BPP (0), Res (1), Com (2)") +
  ylab("Log Improvement Value ($)") +
  labs(color = "Non-Vacant (0) or Vacant (1)")

ggsave(
  filename = "division_cd_scatterplot.png",
  plot = last_plot(),
  path = "../../Figures")

# scatterplots for improvement value, land value
df %>%
  sample_n(1000) %>%
  ggplot(aes(x=log_land_val, y=log_impr_val, color=factor(vac_par))) +
  geom_point(aes(alpha=.5)) +
  ggtitle("Log Improvement Value by Log Land Value by Vacant/Non-Vacant") +
  xlab("Log Land Value ($)") +
  ylab("Log Improvement Value ($)") +
  labs(color = "Non-Vacant (0) or Vacant (1)")

ggsave(
  filename = "log_land_val_scatterplot.png",
  plot = last_plot(),
  path = "../../Figures")

# plot boxplots for log improvement value by vacant/nonvacant
df %>%
  ggplot(aes(x=factor(vac_par), y=log_impr_val, fill=factor(vac_par))) +
  geom_boxplot() +
  ggtitle("Log Improvement Value by Vacant/Non-Vacant") +
  xlab("Non-Vacant (0) or Vacant (1)") +
  ylab("Log Improvement Value ($)") +
  labs(fill = "Non-Vacant (0) or Vacant (1)")

ggsave(
  filename = "log_impr_val_boxplot.png",
  plot = last_plot(),
  path = "../../Figures")