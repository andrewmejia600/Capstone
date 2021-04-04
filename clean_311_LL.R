getwd()
setwd('C:/SMU_Local/data/capstone/DATA')

library(dplyr)
library(tidyverse)


#target file output is 'clean_crime.r'
df <- read.csv('GIS_PACKAGE_FILES_TO_CSV/Clipped_Parcels_by_Dallas_Simple_inner_join_to_Clipped_2019_311.csv', header=TRUE)
glimpse(df)
dim(df)


new_df <- df %>% count(Shape_STAr, Shape_STLe, Acct, sort = TRUE)
new_df$count_of_311 <- new_df$n #relabeling the count from n to something more specific
new_df <- select (new_df,-c(4))
head(new_df)

#scaling the data
x = new_df$count_of_311

new_df$count_of_311_scaled = scale(x, center = FALSE)
head(new_df)
write.csv(new_df, 'clean_311_LL.csv')
