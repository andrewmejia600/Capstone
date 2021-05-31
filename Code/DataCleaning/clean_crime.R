#
# Prepare clean_crime.csv based on City of Dallas crime data

#file_direct = "C:/SMU_Local/data/capstone/Data/GIS_PACKAGE_FILES_TO_CSV/"
file_direct = "/Users/tina/Documents/School/Capstone/Dallas Files/GIS_PACKAGE_FILES_TO_CSV/"

library(dplyr)
library(tidyverse)
library(superml)


df <- read.csv(file=paste0(file_direct,'Clipped_Parcels_by_Dallas_Simple_inner_join_to_Clipped_2019_Group_A_Crime.csv'))
glimpse(df)
dim(df)
head(df)

new_df <- df %>% 
  filter(year_of_incident==2019) %>% 
  group_by(Acct) %>%
  summarise(count_of_crime = n()) %>% # count number of 311 aggregated by account number
  mutate(count_of_crime_scaled = scale(count_of_crime, center = FALSE)[,1]) # scale the data

head(new_df)
write.csv(new_df, '../../Data/clean_crime.csv')
