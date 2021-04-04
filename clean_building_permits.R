# Create a csv of the desired features from building permits

library(dplyr)
library(BBmisc)

file_direct = "/Users/tina/Documents/School/Capstone/Dallas Files/GIS_PACKAGE_FILES_TO_CSV/"

# add building permit features
building_permits = read.csv(file=paste0(file_direct, 'Clipped_Parcels_by_Dallas_Simple_inner_join_to_Clipped_2019_Build_Perm.csv'), stringsAsFactors = FALSE)

building_permit_features = building_permits %>%
  select(Acct, Permit_Type, PermitDate, Mapsco) %>%
  mutate(Permit_Date = as.Date(substr(PermitDate, 1, 10))) %>%
  group_by(Acct) %>%
  add_tally(name="Count_Permits") %>%
  filter(PermitDate == max(PermitDate)) %>%
  ungroup() %>%
  mutate(Count_Permits = normalize(Count_Permits, method="scale")) %>%
  mutate(Days_Since_Permit =  normalize(as.numeric(Sys.Date() - Permit_Date, units="days"), method="scale")) %>%
  mutate(Permit_Type = as.numeric(factor(Permit_Type))) %>%
  select(Acct, Permit_Type, Count_Permits, Days_Since_Permit)


# Write the data set to a file
write.csv(building_permit_features, "Data/Derived/df_building_permits.csv")
