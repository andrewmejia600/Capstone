# Create a data set for machine learning with all the desired features

library(dplyr)

# file_direct = '/home/andrew/Desktop/Dallas Files/GIS_PACKAGE_FILES_TO_CSV/'
file_direct = "/Users/tina/Documents/School/Capstone/Dallas Files/GIS_PACKAGE_FILES_TO_CSV/"

# Start the data set with simple Dallas parcels
df = read.csv(file=paste0(file_direct,'Clipped_Parcels_by_Dallas_Simple.csv'), stringsAsFactors = FALSE)

# CPAL Annotations
CPAL_label_annotations = read.csv(file=paste0(file_direct,'Clipped_Parcels_by_Dallas_Simple_vac_pts_annotations.csv'), stringsAsFactors = FALSE)
CPAL_label_annotations["VAC_PAR"] = 1


duplicate_accounts = c()

df = df %>% 
    filter(!(Acct %in% duplicate_accounts)) %>%
    left_join(CPAL_label_annotations, by = c("Acct" = "ACCOUNT_NUM")) 
#    left_join(CO) %>%
#    left_join(Building_permits) %>%
#    left_join(DCAD) %>%
#    left_join(data311) 

  
  

# Write the data set to a file
write.csv(df, "dataset.csv")
