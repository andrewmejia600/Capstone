# Create a csv of the desired features from building permits

library(dplyr)

file_direct = "/Users/tina/Documents/School/Capstone/Dallas Files/GIS_PACKAGE_FILES_TO_CSV/"

# add building permit features
building_permits = read.csv(file=paste0(file_direct, 'Clipped_Parcels_by_Dallas_Simple_inner_join_to_Clipped_2019_Build_Perm.csv'), stringsAsFactors = FALSE, nrows=10000)

building_permit_features = building_permits %>%
  select(Acct, Permit_Type, PermitDate, Mapsco) %>%
  mutate(Permit_Date = as.Date(substr(PermitDate, 1, 10))) %>%
  group_by(Acct) %>%
  add_tally(name="Count_Permits") %>%
  filter(PermitDate == max(PermitDate)) %>%
  mutate(Days_Since_Permit =  Sys.Date() - Permit_Date) %>%
  select(Acct, Permit_Type, Mapsco, Count_Permits, Days_Since_Permit)


# Write the data set to a file
write.csv(building_permit_features, "Data/Derived/df_building_permits.csv")
