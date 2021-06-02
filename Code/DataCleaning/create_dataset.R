#
# Create a master dataset for classifying parcels in the City of Dallas

library(dplyr)
library(BBmisc)

# gis_file_direct = '/home/andrew/Desktop/Dallas Files/GIS_PACKAGE_FILES_TO_CSV/'
# gis_file_direct = "/Users/tina/Documents/School/Capstone/Dallas Files/GIS_PACKAGE_FILES_TO_CSV/"
gis_file_direct = "C:/SMU_Local/data/capstone/Data/GIS_PACKAGE_FILES_TO_CSV/"

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

# Join labels, vac_par=1 to df  Note, this also eliminates any duplicate Acct values from CPAL_labels
df_with_labels = left_join(df,CPAL_labels, by = c("Acct" = "Acct"), keep = FALSE)

### Add remaining target labels, vac_par=0
df_with_labels['vac_par'] = ifelse(is.na(df_with_labels$vac_par),0,1)

summary(df_with_labels)  #No NAs in vac_par

table(df_with_labels$vac_par)  #264459 = 0 28562 =1

df = df_with_labels

###############  Join All prepared dataframes to reference df

# Read dataframes that were provided by different sources and pre-processed

# Note: clean_DCAD.csv is too large for github so it must be located in gis_file_direct

df_DCAD = read.csv(paste0(gis_file_direct, 'clean_DCAD.csv'), stringsAsFactors = FALSE)

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
#new_df$count_of_311 <- scale(x, center = FALSE)



# Remove unnecessary columns for modeling
# GIS_parcel_ID, Shape*, X vals
# c(1:5,7, 20:22, 25, 31, 43:46)
excluded_vars = c("fid", "RecAcs.x", "Shape_STAr.x", "Shape_STLe.x", "gis_parcel_id",
  "X.x", "Shape_STAr.y", "Shape_STLe.y", "X.y", "X.x.x", "X.y.y", "Shape_STAr",
  "Shape_STLe", "RecAcs.y")
df_all <- select(df_complete,-one_of(excluded_vars))

summary(df_all)


df_log_and_scaled <- df_all %>%
  select(Acct,vac_par, num_sptd, num_division_cd, num_nbhd_cd, zoning_buckets, log_impr_val, log_land_val, log_tot_val,
         log_area_sqft, count_of_311_scaled, permit_type, count_permits_scaled, days_since_permit_scaled,
         days_from_CO_appro_to_issue_scaled, count_COs_scaled, days_since_issue_scaled, CO_type, sq_ft_scaled,
         occupancy, CO_code_distr, nibrs_crime_against, nibrs_crime_against, count_of_crime_scaled)
df_not_scaled <- df_all %>%
  select(Acct,vac_par, num_sptd, num_division_cd, num_nbhd_cd, zoning_buckets, impr_val, land_val, tot_val,
         area_sqft, count_of_311, permit_type, count_permits, days_since_permit,
         days_from_CO_appro_to_issue, count_COs, days_since_issue, CO_type, sq_ft,
         occupancy, CO_code_distr, nibrs_crime_against, nibrs_crime_against, count_of_crime)
df_all_with_Acct_GIS <- cbind("Acct"=df_complete$Acct,"GIS_parcel_ID"=df_complete$gis_parcel_id,df_all)

# print out whether any values are NaN or NA or -Inf or Inf
# if any of these are >0, fix something
colSums(!sapply(df_log_and_scaled, is.finite))

# Write the data set to a file
write.csv(df_all, "../../Data/df_all.csv", row.names = FALSE)
write.csv(df_log_and_scaled,"../../Data/df_log_and_scaled.csv", row.names = FALSE)
write.csv(df_not_scaled,"../../Data/df_not_scaled.csv", row.names = FALSE)
write.csv(df_all_with_Acct_GIS,"../../Data/df_all_with_Acct_GIS.csv", row.names = FALSE)

######################  End create dataset

 
