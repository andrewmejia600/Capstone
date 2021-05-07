#
# Create a master dataset for classifying parcels in the City of Dallas

library(dplyr)
library(BBmisc)

# gis_file_direct = '/home/andrew/Desktop/Dallas Files/GIS_PACKAGE_FILES_TO_CSV/'
gis_file_direct = "/Users/tina/Documents/School/Capstone/Dallas Files/GIS_PACKAGE_FILES_TO_CSV/"
# file_direct = "C:/SMU_Local/data/capstone/Data/GIS_PACKAGE_FILES_TO_CSV/"

#Begin with shape layer of all parcels in Dallas County, limited to the City of Dallas
dallas_simple = read.csv(file=paste0(gis_file_direct,'Clipped_Parcels_by_Dallas_Simple.csv'), stringsAsFactors = FALSE)

summary(dallas_simple)

# Build an array of Accts that are associated with more than one parcel

dallas_multiple_accounts = dallas_simple %>%
  group_by(Acct) %>%
  count() %>%
  filter(n>1) 

summary(dallas_multiple_accounts)  # Answer - yes:  35 rows, 2 columns "Acct" and "n"

# Print out all 35 Acct values to see any trend or pattern in ownership

dallas_multiple_accounts %>% print(n = Inf)

# Assess total parcels owned by 35 Acct owners

mult_Acct = sum(dallas_multiple_accounts$n)  # 35 Accounts are associated with 373 parcels


# Remove ambiguous Acct values from dallas_simple

dallas_simple_unique = dallas_simple %>% filter(!(dallas_simple$Acct %in% dallas_multiple_accounts$Acct))

# df is based on dallas_simple_unique

df = dallas_simple_unique
summary(df)  # 293020 Unique Acct Values in City of Dallas shape file

#######################  End df Create - no Annotations

###################### Add Annotations from DCAD_vac_pts.csv which was provided by CPAL

CPAL_labels = read.csv('../../Data/DCAD_vac_pts.csv', stringsAsFactors = FALSE)
# Create Annotation factor based on the Acct being a member of the list of Accounts in DCAD_vac_pts.csv
CPAL_labels["vac_par"] = 1

# Remove attributes from DCAD_vac_pts.csv except Acct and vac_par
CPAL_labels = data.frame("Acct"=CPAL_labels$ACCOUNT_NUM, "vac_par" = CPAL_labels$vac_par)

# For future reference:  Detect ambiguous Acct values in DCAD_vac_pts.csv
# Note, this is not used at this time, since these accounts are not in df and left join should filter them
CPAL_multiple_accounts = CPAL_labels %>%
  group_by(Acct) %>%
  count() %>%
  filter(n>1)

# Join labels, vac_par=1 to df
df_with_labels = left_join(df,CPAL_labels, by = c("Acct" = "Acct"), keep = FALSE)

### Add remaining target labels, vac_par=0
df_with_labels['vac_par'] = ifelse(is.na(df_with_labels$vac_par),0,1)

summary(df_with_labels)  #No NAs in vac_par

table(df_with_labels$vac_par)  #264459 = 0 28562 =1

df = df_with_labels

###############  Join All prepared dataframes to reference df

# Read dataframes that were provided by different sources and pre-processed

df_DCAD = read.csv('../../Data/clean_DCAD.csv', stringsAsFactors = FALSE)
df_311 = read.csv('../../Data/clean_311.csv', stringsAsFactors = FALSE)
df_bp = read.csv('../../Data/clean_building_permits.csv', stringsAsFactors = FALSE)
df_CO = read.csv('../../Data/clean_cert_occupancy.csv', stringsAsFactors = FALSE)
df_crime = read.csv('../../Data/clean_crime.csv', stringsAsFactors = FALSE)

df_complete = df %>% 
    left_join(df_DCAD, by = c("Acct" = "Acct"), keep = FALSE) %>%
    left_join(df_311, by = c("Acct" = "Acct"), keep = FALSE) %>%
    left_join(df_bp, by = c("Acct" = "Acct"), keep = FALSE) %>%
    left_join(df_CO, by = c("Acct" = "Acct"), keep = FALSE) %>%
    left_join(df_crime, by = c("Acct" = "Acct"), keep = FALSE)
  
summary(df_complete)

# NO NA values in target vac_par
# Replace all NA values with 0
df_complete[is.na(df_complete)] <- 0

summary(df_complete)

# Normalize DCAD numeric factors.

df_complete$impr_val_scaled = normalize(df_complete$impr_val, method="scale")
df_complete$land_val_scaled = normalize(df_complete$land_val, method="scale")
df_complete$tot_val_scaled = normalize(df_complete$tot_val, method="scale")
df_complete$area_sqft_scaled = normalize(df_complete$area_sqft, method="scale")


# Remove unnecessary columns for modeling
# Acct, GIS_parcel_ID, Shape*, X vals

df_all <- select(df_complete,-c(1:8,12,13,21:23,26,32,44:47)) 
df_all$vac_par = df_complete$vac_par  # Append vac_par annotations to end of dataframe

summary(df_all)


df_log_and_scaled <- select(df_all, c(7:10,12,13,15,17,19,21,23,24,26:30,32:37))
df_not_scaled <- select(df_all, c(1:6,11,13,14,16,18,20,22,24,25,27:31,37))
df_all_with_Acct_GIS <- cbind("Acct"=df_complete$Acct,"GIS_parcel_ID"=df_complete$gis_parcel_id,df_all)

# Write the data set to a file
write.csv(df_all, "../../Data/df_all.csv")
write.csv(df_log_and_scaled,"../../Data/df_log_and_scaled.csv")
write.csv(df_not_scaled,"../../Data/df_not_scaled.csv")
write.csv(df_all_with_Acct_GIS,"../../Data/df_all_with_Acct_GIS.csv")

######################  End create dataset

 
