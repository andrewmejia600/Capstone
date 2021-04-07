
#  Clean 311 Code data from City of Dallas, TX

library(dplyr)
library(tidyverse)
file_direct = 'C:/SMU_Local/data/capstone/Data/GIS_PACKAGE_FILES_TO_CSV/'

df = read.csv(file=paste0(file_direct,'Clipped_Parcels_by_Dallas_Simple_inner_join_to_Clipped_2019_311.csv'))

new_df <- df %>% count(Shape_STAr, Shape_STLe, Acct, sort = TRUE)
new_df$count_of_311 <- new_df$n #relabeling the count from n to something more specific
new_df <- select (new_df,-c(4))
head(new_df)

#scaling the data
x = new_df$count_of_311

new_df$count_of_311_scaled = scale(x, center = FALSE)
head(new_df)
write.csv(new_df, '../../Data/clean_311.csv')
