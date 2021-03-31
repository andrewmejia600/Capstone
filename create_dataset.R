# Create a data set for machine learning with all the desired features

library(dplyr)

# file_direct = '/home/andrew/Desktop/Dallas Files/GIS_PACKAGE_FILES_TO_CSV/'
file_direct = "/Users/tina/Documents/School/Capstone/Dallas Files/GIS_PACKAGE_FILES_TO_CSV/"

# Start the data set with simple Dallas parcels
df = read.csv(file=paste0(file_direct,'Clipped_Parcels_by_Dallas_Simple.csv'), stringsAsFactors = FALSE)

# CPAL Annotations
CPAL_label_annotations = read.csv(file=paste0(file_direct,'Clipped_Parcels_by_Dallas_Simple_vac_pts_annotations.csv'), stringsAsFactors = FALSE)
CPAL_label_annotations["VAC_PAR"] = 1

# all the dcad data from which to obtain a list of duplicate accounts
DCAD_all = read.csv("my_dcad_file.csv")
DCAD_all = DCAD_all %>%
  select("Acct") %>%
  rbind("other_dcad_data")

# Make list of accounts that have no 
duplicate_accounts = CPAL_label_annotations %>%
  group_by(ACCOUNT_NUM) %>%
  count() %>%
  filter(n>1) 

# add building permit features
building_permits = read.csv(file=paste0(file_direct, 'Clipped_Parcels_by_Dallas_Simple_inner_join_to_Clipped_2019_Build_Perm.csv'), stringsAsFactors = FALSE, nrows=10000)
building_permit_features = building_permits %>%
  select(Acct, Permit_Type, PermitDate, Mapsco) %>%
  mutate(Permit_Date = as.Date(substr(PermitDate, 1, 10))) %>%
  group_by(Acct) %>%
  add_tally(name="Count_Permits") %>%
  filter(PermitDate == max(PermitDate)) %>%
  mutate(Days_Since_Permit =  Sys.Date() - Permit_Date)

df = df %>% 
    filter(!(Acct %in% duplicate_accounts$Acct)) %>%
    left_join(CPAL_label_annotations, by = c("Acct" = "ACCOUNT_NUM")) %>%
    left_join(building_permit_features, by = "Acct")
#    left_join(CO) %>%
#    left_join(DCAD) %>%
#    left_join(data311) 

  
  

# Write the data set to a file
write.csv(df, "dataset.csv")
