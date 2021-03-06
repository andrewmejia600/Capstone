# Create a csv of the desired features from building permits

library(dplyr)
library(BBmisc)

file_direct = "/Users/tina/Documents/School/Capstone/Dallas Files/GIS_PACKAGE_FILES_TO_CSV/"
#file_direct = 'C:/SMU_Local/data/capstone/Data/GIS_PACKAGE_FILES_TO_CSV/'

# add building permit features
building_permits = read.csv(file=paste0(file_direct, 'Clipped_Parcels_by_Dallas_Simple_inner_join_to_Clipped_2019_Build_Perm.csv'), stringsAsFactors = FALSE)

building_permit_features = building_permits %>%
  select(Acct, Permit_Type, PermitDate, Mapsco) %>%
  mutate(Permit_Date = as.Date(substr(PermitDate, 1, 10))) %>%
  group_by(Acct) %>%
  add_tally(name="count_permits") %>%
  filter(PermitDate == max(PermitDate)) %>%
  ungroup() %>%
  mutate(days_since_permit =  as.numeric(as.Date("2021-01-01") - Permit_Date, units="days")) %>%
  mutate(permit_type = as.numeric(factor(Permit_Type))) %>%
  group_by(Acct) %>%
  summarise(permit_type=median(permit_type), count_permits=median(count_permits), 
            days_since_permit=median(days_since_permit)) %>%
  mutate(count_permits_scaled = normalize(count_permits, method="scale")) %>%
  mutate(days_since_permit_scaled =  normalize(days_since_permit, method="scale")) %>%
  select(Acct, permit_type, count_permits, count_permits_scaled, days_since_permit, days_since_permit_scaled)

account_counts = building_permit_features %>% count(Acct) %>% arrange(desc(n))
print(head(account_counts))

# Write the data set to a file
write.csv(building_permit_features, "../../Data/clean_building_permits.csv")
