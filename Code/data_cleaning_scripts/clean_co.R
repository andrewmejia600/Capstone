# Create a csv of the desired features from certificate of occupancy

library(dplyr)
library(BBmisc)

# add co features
cert_occupancy = read.csv(file=paste0(file_direct, 'Clipped_Parcels_by_Dallas_Simple_inner_join_to_Clipped_2019_co.csv'), stringsAsFactors = FALSE)
cert_occupancy_features = cert_occupancy %>%
  mutate(CO_Issue_Date = as.Date(ISSUE.DATE, "%m/%d/%Y")) %>%
  mutate(Days_From_CO_Appro_To_Issue = normalize(as.numeric(CO_Issue_Date - as.Date(DATE.APPRO, "%m/%d/%Y"), units="days"), method="scale")) %>%
  mutate(Days_Since_Issue = normalize(as.numeric(Sys.Date() - CO_Issue_Date, units="days"), method="scale")) %>%
  mutate(Sq_Ft = normalize(SQ.FT, method="scale")) %>%
  group_by(Acct) %>%
  add_tally(name="Count_COs") %>%
  ungroup() %>%
  mutate(Count_COs = normalize(Count_COs, method="scale")) %>%
  filter(CO_Issue_Date == max(CO_Issue_Date)) %>%
  mutate(Occupancy = as.numeric(factor(OCCUPANCY))) %>%
  mutate(CO_Code_Distr = as.numeric(factor(CODE.DISTR))) %>%
  mutate(CO_Type = as.numeric(factor(TYPE.OF.CO))) %>%
  select(Acct, Days_From_CO_Appro_To_Issue, Count_COs, Days_Since_Issue, CO_Type, Sq_Ft, Occupancy, CO_Code_Distr)

# Write the data set to a file
write.csv(cert_occupancy_features, "Data/Derived/df_cert_occupancy.csv")
