#
# Prepare clean_crime.csv based on City of Dallas crime data

file_direct = "C:/SMU_Local/data/capstone/Data/GIS_PACKAGE_FILES_TO_CSV/"

library(dplyr)
library(tidyverse)
library(superml)


df <- read.csv(file=paste0(file_direct,'Clipped_Parcels_by_Dallas_Simple_inner_join_to_Clipped_2019_Group_A_Crime.csv'))
glimpse(df)
dim(df)
head(df)

new_df <- df %>% filter(year_of_incident==2019) %>% count(Shape_STAr, Shape_STLe, Acct, RecAcs, 
             nibrs_crime_category,	nibrs_crime_against, sort = TRUE)


new_df$count_of_crime <- new_df$n #relabeling the count from n to something more specific
new_df <- select (new_df,-c(7)) #dropping n column

####Label encoding
label <- LabelEncoder$new()

new_df$nibrs_crime_category <- label$fit_transform(new_df$nibrs_crime_category)
new_df$nibrs_crime_against <- label$fit_transform(new_df$nibrs_crime_against)

#scaling the data
x = new_df$count_of_crime
new_df$count_of_crime_scaled <- scale(x, center = FALSE)

write.csv(new_df, '../../Data/clean_crime.csv')
