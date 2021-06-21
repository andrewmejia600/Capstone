library(ggplot2)
library(dplyr)

read_data = read.csv('https://raw.githubusercontent.com/andrewmejia600/Capstone/main/Data/df_all_with_Acct_GIS.csv')

df = read_data
df = df %>% filter(num_division_cd != 0)
df$num_division_cd = ifelse(df$num_division_cd == 1, 'RES', 
                            ifelse(df$num_division_cd == 2, 'COM', 'BPP'))

df['Class'] = factor(ifelse(df$vac_par == 1, "VAC", "NVAC"))


df_gg = df %>%
  filter(impr_val < 37794550) %>%
  group_by(GIS_parcel_ID) %>%
  summarise(num_division_cd = max(num_division_cd))
head(df_gg)

p = df_gg %>% 
  count(division = factor(num_division_cd)) %>% 
  mutate(pct = prop.table(n)) %>%
  ggplot(aes(x = division, y = pct, fill = division, label = paste(scales::percent(pct), "\n ( n=", n, ")"))) + 
  geom_col(aes(color = division),position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = .5,    # nudge above top of bar
            size = 3.75) + 
  scale_y_continuous(labels = scales::percent) + 
  ggtitle("Percent Parcels by Division \n (Residential, Commercial, and Business Personal Property)") + 
  theme_minimal() + theme(text = element_text(size=15), 
         axis.title.x = element_text(size=14, face = "bold"),
         axis.title.y = element_text(size=14, face = "bold")) + 
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0))

p + scale_fill_manual(values=c("#999999", "#325860", "#FFA533"))


p = df %>%
  sample_n(1000) %>%
  ggplot(aes(x=factor(num_division_cd), y=log_impr_val, color=factor(Class))) +
  geom_point(position = "jitter", alpha = .5) +
  ggtitle("Log Improvement Value by Division Code by Vacant/Non-Vacant") +
  ylab("Log Improvement Value ($)") +
  labs(color = "Class", x = "") + 
  theme_minimal() + theme(text = element_text(size=14), 
                          axis.title.x = element_text(size=14, face = "bold"),
                          axis.title.y = element_text(size=14, face = "bold")) + 
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0))

p + scale_color_manual(values=c("#325860", "#FFA533"))

p = df %>%
  sample_n(1000) %>%
  ggplot(aes(x=log_land_val, y=log_impr_val, color=factor(Class))) +
  geom_point(alpha = .5) +
  ggtitle("Log Improvement Value by Log Land Value by Vacant/Non-Vacant") +
  xlab("Log Land Value ($)") +
  ylab("Log Improvement Value ($)") +
  labs(color = "Class") + 
  theme_minimal() + theme(text = element_text(size=14), 
                          axis.title.x = element_text(size=14, face = "bold"),
                          axis.title.y = element_text(size=14, face = "bold")) + 
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0))

p + scale_color_manual(values=c("#325860", "#FFA533"))


df_gg = df %>%
  filter(!is.na(impr_val)) %>%
  filter(impr_val < 37794550) %>%
  filter(num_division_cd != "BPP") %>%
  group_by(GIS_parcel_ID) %>%
  summarise(impr_val = max(impr_val)) %>%
  mutate(lt_1k_impr_val = ifelse(impr_val < 1000, "Yes", "No"))
head(df_gg)
summary(df_gg$impr_val)

p = df_gg %>% 
  count(lt_1k_impr_val = factor(lt_1k_impr_val)) %>% 
  mutate(pct = prop.table(n)) %>%
  ggplot(aes(x = lt_1k_impr_val, y = pct, fill = lt_1k_impr_val, label = paste(scales::percent(pct), "\n ( n=", n, ")"))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = .5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  ggtitle("Percent Parcels Less Than $1k Improvement Value") + 
  theme_minimal() + theme(text = element_text(size=15), 
                          axis.title.x = element_text(size=14, face = "bold"),
                          axis.title.y = element_text(size=14, face = "bold")) + 
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0))

p + scale_fill_manual(values=c("#325860", "#FFA533"))
