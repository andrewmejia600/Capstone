### Create graphs and exploratory data analysis on top 3 features improvement val
### land val, and division code

library(dplyr)
library(ggplot2)

df = read.csv("../../Data/df_all.csv")
# Plot count and percent of division (residential, commercial, bpp)
# aggregate rows to one row per GIS_PARCEL_ID

groupColors = c("#CC79A7", "#325860", "#FFA533")

df_gg %>% 
  count(division = factor(num_division_cd)) %>% 
  mutate(pct = prop.table(n)) %>%
  ggplot(aes(x = division, y = pct, fill = division, label = paste(scales::percent(pct), "\n ( n=", n, ")"))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = .5,    # nudge above top of bar
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  ggtitle("Percent Parcels by Division \n (Residential, Commercial, \n and Business Personal Property)") +
  xlab("Division (BPP=0, Res=1, Com=2)") +
  scale_fill_manual(values=groupColors)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title=element_text(size=22, face="bold"),
        legend.title=element_text(size=12),
        panel.spacing = units(2))

# plot boxplots for log improvement value by vacant/nonvacant
df %>%
  ggplot(aes(x=factor(num_division_cd), y=log_impr_val, fill=factor(vac_par))) +
  geom_point() +
  ggtitle("Zoning by Vacant/Non-Vacant") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave(
  filename = "zoning_proportion_barchart.png",
  plot = last_plot(),
  path = "../../Figures/")
# Plot count and percent of parcels with <$1k improvement value vs >$1k

# aggregate rows to one row per GIS_PARCEL_ID with max improvement value

df = df_all

df_gg = df %>%
  filter(!is.na(impr_val)) %>%
  filter(impr_val < 37794550) %>%
  mutate(lt_1k_impr_val = ifelse(impr_val < 1000, "yes", "no"))

head(df_gg)
summary(df_gg$impr_val)

df_gg %>% 
  count(lt_1k_impr_val = factor(lt_1k_impr_val)) %>% 
  mutate(pct = prop.table(n)) %>%
  ggplot(aes(x = lt_1k_impr_val, y = pct, fill = lt_1k_impr_val, label = paste(scales::percent(pct), "\n ( n=", n, ")"))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = .5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  ggtitle("Percent Parcels Less Than $1k Improvement Value")