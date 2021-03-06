# Create a csv of the desired features from certificate of occupancy

library(dplyr)
library(BBmisc)

file_direct = "C:/SMU_Local/data/capstone/Data/GIS_PACKAGE_FILES_TO_CSV/"

# add co features
cert_occupancy = read.csv(file=paste0(file_direct, 'Clipped_Parcels_by_Dallas_Simple_inner_join_to_Clipped_2019_co.csv'), stringsAsFactors = FALSE)
cert_occupancy_features = cert_occupancy %>%
  mutate(CO_Issue_Date = as.Date(ISSUE.DATE, "%m/%d/%Y")) %>%
  group_by(Acct) %>%
  add_tally(name="count_COs") %>%
  filter(CO_Issue_Date == max(CO_Issue_Date)) %>%
  ungroup() %>%
  mutate(days_from_CO_appro_to_issue = as.numeric(CO_Issue_Date - as.Date(DATE.APPRO, "%m/%d/%Y"), units="days")) %>%
  mutate(days_since_issue = as.numeric(as.Date("2021-01-01") - CO_Issue_Date, units="days")) %>%
  mutate(CO_sqft = SQ.FT) %>%
  mutate(occupancy = as.numeric(factor(OCCUPANCY))) %>%
  mutate(CO_code_distr = as.numeric(factor(CODE.DISTR))) %>%
  mutate(CO_type = as.numeric(factor(TYPE.OF.CO))) %>%  
  group_by(Acct) %>%
  summarise(days_from_CO_appro_to_issue=median(days_from_CO_appro_to_issue), 
            count_COs=median(count_COs), days_since_issue=median(days_since_issue), CO_type=median(CO_type),
            CO_sqft=sum(CO_sqft), occupancy=median(occupancy), CO_code_distr=median(CO_code_distr)) %>%
  mutate(days_from_CO_appro_to_issue_scaled = normalize(days_from_CO_appro_to_issue, method="scale")) %>%
  mutate(days_since_issue_scaled = normalize(days_since_issue, method="scale")) %>% 
  mutate(CO_sqft_scaled = normalize(CO_sqft, method="scale")) %>%
  mutate(count_COs_scaled = normalize(count_COs, method="scale")) %>%
  select(Acct, days_from_CO_appro_to_issue, days_from_CO_appro_to_issue_scaled, count_COs, count_COs_scaled, 
            days_since_issue, days_since_issue_scaled, CO_type, CO_sqft, CO_sqft_scaled, occupancy, CO_code_distr)

# Write the data set to a file
write.csv(cert_occupancy_features, "../../Data/clean_cert_occupancy.csv")
