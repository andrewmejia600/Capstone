
#  Clean 311 Code data from City of Dallas, TX

library(dplyr)
library(tidyverse)
file_direct = 'C:/SMU_Local/data/capstone/Data/GIS_PACKAGE_FILES_TO_CSV/'

df = read.csv(file=paste0(file_direct,'Clipped_Parcels_by_Dallas_Simple_inner_join_to_Clipped_2019_311.csv'))

new_df <- df %>% 
  group_by(Acct) %>%
  summarise(count_of_311 = n()) %>% # count number of 311 aggregated by account number
  mutate(count_of_311_scaled = scale(count_of_311, center = FALSE)[,1]) # scale the data
  
head(new_df)

write.csv(new_df, '../../Data/clean_311.csv')
