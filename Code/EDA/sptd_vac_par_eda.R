#
# Create comparison of vac_par to sptd
#
###########
library(ggmosaic)

df = read.csv("../../Data/df_not_scaled.csv")

df_gg = df %>%
  mutate(sptd_vac_par = factor(ifelse(num_sptd==1, 1, 0))) %>%
  mutate(vac_par = factor(vac_par))


ggplot(df_gg) +
  geom_mosaic(aes(x=product(vac_par, sptd_vac_par), fill = vac_par)) + 
  labs(y="CPAL Labeled Vacant", x = "Dallas Labeled Vacant", title = "CPAL vs Dallas Vacant Lot Labels", fill="CPAL Labeled Vacant")
  
ggsave(
  filename = "sptd_cpal_vacant_mosaic.png",
  plot = last_plot(),
  path = "../../Figures/")
