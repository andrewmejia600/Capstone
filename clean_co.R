# Create a csv of the desired features from certificate of occupancy

# add co features
cert_occupancy = read.csv(file=paste0(file_direct, 'Clipped_Parcels_by_Dallas_Simple_inner_join_to_Clipped_2019_co.csv'), stringsAsFactors = FALSE)
cert_occupancy_features = cert_occupancy %>%
  mutate(CO_Issue_Date = as.Date(ISSUE.DATE, "%m/%d/%Y")) %>%
  mutate(Days_From_CO_Appro_To_Issue = CO_Issue_Date - as.Date(DATE.APPRO, "%m/%d/%Y")) %>%
  group_by(Acct) %>%
  add_tally(name="Count_COs") %>%
  filter(CO_Issue_Date == max(CO_Issue_Date)) %>%
  mutate(Days_Since_Issue =  Sys.Date() - CO_Issue_Date) %>%
  select(Acct, Days_From_CO_Appro_To_Issue, Count_COs, Days_Since_Issue, CO_Type=TYPE.OF.CO, Sq_Ft=SQ.FT, Occupancy=OCCUPANCY, CO_Code_Distr=CODE.DISTR)

# Write the data set to a file
write.csv(cert_occupancy_features, "Data/Derived/df_cert_occupancy.csv")
