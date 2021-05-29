

########################
# Read, combine and prepare data from Dallas Central Appraisal District (DCAD)

# account_apprl_year.csv
# land.csv
########################


library(dplyr)
library(tidyverse)
#file_direct = "C:/SMU_Local/data/capstone/Data/DCAD2019_CURRENT/"
file_direct = "/Users/tina/Documents/School/Capstone/Dallas Files/DCAD2019_CURRENT/"
gis_file_direct = "/Users/tina/Documents/School/Capstone/Dallas Files/GIS_PACKAGE_FILES_TO_CSV/"

#####################  Prepare land.csv 

land = read.csv(file=paste0(file_direct,'land.csv'), stringsAsFactors = FALSE)



#Rebucket land zoning
land = land %>%
  mutate(zoning_buckets = ifelse(str_detect(ZONING, "FAMILY"), "family",
                                 ifelse(str_detect(ZONING, "PLANNED DEVELOPMENT"), "development",
                                        ifelse(str_detect(ZONING, "INDUSTRIAL"), "industrial",
                                               ifelse(str_detect(ZONING, "OFFICE"), "office",
                                                      ifelse(str_detect(ZONING, "AGRICULTURAL"), "agricultural",
                                                             ifelse(str_detect(ZONING, "RETAIL"), "retail",
                                                                    ifelse(str_detect(ZONING, "LIQUOR"), "liquor","other")))))))) %>%
  mutate(zoning_buckets = as.numeric(factor(zoning_buckets)))

summary(land)

# New Feature:  AREA_SQFT
# Calculate total area of parcel in square feet.  Some parcels are measured in acres.

l=as.numeric(length(land$AREA_SIZE))
land$AREA_UOM_DESC=as.character(land$AREA_UOM_DESC)

land = land %>%
  mutate(AREA_SQFT = ifelse(AREA_UOM_DESC=="ACRE", AREA_SIZE*43560,
                            ifelse(AREA_UOM_DESC=="UNASSIGNED", FRONT_DIM*DEPTH_DIM, 
                                   AREA_SIZE)))

## Reduce land dataset to factors we will analyze

df_land = data.frame(land$ACCOUNT_NUM, land$AREA_SQFT, land$zoning_buckets)
names(df_land) <- c('Acct','area_sqft', 'zoning_buckets')

## Eliminate multiple Acct values before joining with other DCAD files

# Create an array of Acct where count>1 and remove them from df_land
land_multiple_accounts = df_land %>%
  group_by(Acct) %>%
  count() %>%
  filter(n>1) 

summary(land_multiple_accounts)  # 3381 Acct values ranging from 2 to 17

mult_Acct_land = sum(land_multiple_accounts$n)  # 6913 total parcels affected


# Remove ambiguous Acct values from df_land

df_land_unique = df_land %>% filter(!(df_land$Acct %in% land_multiple_accounts$Acct))

summary(df_land_unique)  #718251 observations

print(df_land_unique %>% count(Acct) %>% arrange(desc(n))) #there are no duplicate account numbers now

########################## Prepare ACCOUNT_APPRL_YEAR.csv

### This file is the primary DCAD file and is the only dataset that includes GIS_Parcel_ID.

apprl = read.csv(file=paste0(file_direct,'account_apprl_year.csv'), stringsAsFactors = FALSE)

# Reduce appraisal factors to those we will analyze
df_apprl = data.frame(apprl$ACCOUNT_NUM, apprl$GIS_PARCEL_ID, apprl$IMPR_VAL, apprl$LAND_VAL, apprl$TOT_VAL, apprl$DIVISION_CD, apprl$SPTD_CODE)
names(df_apprl) <- c('Acct','gis_parcel_id','impr_val','land_val','tot_val','division_cd','sptd_code')
summary(df_apprl)

# Analyze duplicate Acct Values - no ambiguous Acct values in account_apprl_year.csv
apprl_multiple_accounts = df_apprl %>%
  group_by(Acct) %>%
  count() %>%
  filter(n>1) 

# Convert division_cd to numeric labels.  RES = 1, COM = 2, BPP = 3

barplot(table(df_apprl$division_cd))  #commercial, residential and business personal property bar chart

df_apprl$num_division_cd = ifelse(df_apprl$division_cd == 'RES', 1, 
       ifelse(df_apprl$division_cd == 'COM', 2, 3))

# delete the original text version of the column
df_apprl$division_cd = NULL


# Evaluate and bin sptd_code = State Property Tax Division Code values

# sptd_code conversion 
# 
# C* and O10 = 1 = vacant
# A* = 2 = single family residential
# B* = 3 = multi-family
# D* = 4 = agricultural and non-qualified
# E*, F*, G*, M*, N*, O11 = 5 = Improvements, Mobile Homes, Intangibles and Mineral Rights
# J* = 6 = companies
# L* = 7 = business personal property
# S* = 8 = special inventory

x=as.character(df_apprl$sptd_code)

df_apprl$num_sptd = ifelse(startsWith(x,'C'),1,
                           ifelse((x=='O10'),1,
                                  ifelse(startsWith(x,'A'),2,
                                         ifelse(startsWith(x,'B'),3,
                                                ifelse(startsWith(x,'D'),4,
                                                       ifelse(startsWith(x,'E'),5,
                                                              ifelse(startsWith(x,'F'),5,
                                                                     ifelse(startsWith(x,'G'),5,
                                                                            ifelse(startsWith(x,'M'),5,
                                                                                   ifelse(startsWith(x,'N'),5,
                                                                                          ifelse((x=='O11'),5,
                                                                                          ifelse(startsWith(x,'J'),6,
                                                                                                 ifelse(startsWith(x,'L'),7,8)))))))))))))


# delete the original text version of the column
df_apprl$sptd_code = NULL

barplot(table(df_apprl$num_sptd))


########################## Prepare ACCOUNT_INFO.csv

### This file has additional information about each account.

account_info = read.csv(file=paste0(file_direct,'account_info.csv'), stringsAsFactors = FALSE)

#keeping nbhd_cd, the only feature of interest
df_account_info = account_info %>%
  mutate(num_nbhd_cd = as.numeric(factor(NBHD_CD))) %>%
  select(Acct = ACCOUNT_NUM, NBHD_CD, num_nbhd_cd)

print(df_account_info %>% count(Acct) %>% arrange(desc(n))) # no duplicate accounts

##################### Join datasets to create df_DCAD


df_DCAD = left_join(df_apprl,df_land_unique, by = c("Acct" = "Acct"), keep = FALSE) %>%
  left_join(df_account_info, by = c("Acct" = "Acct")) %>%
  distinct()

# Log (base e) Normalize value factors and area_sqft - range of values is extreme.

df_DCAD$log_impr_val = log(df_DCAD$impr_val + 1)
df_DCAD$log_land_val = log(df_DCAD$land_val + 1)
df_DCAD$log_tot_val = log(df_DCAD$tot_val + 1)
df_DCAD$log_area_sqft = log(df_DCAD$area_sqft + 1)

df_DCAD_acct_counts = df_DCAD %>% count(Acct) %>% arrange(desc(n)) 
print(head(df_DCAD_acct_counts)) 

# Note:  Values include parcels outside of City of Dallas.
# DCAD continuous factors will be scaled after left join with df in create_dataset.r

write.csv(df_DCAD, paste0(gis_file_direct, 'clean_DCAD.csv'),quote = TRUE, row.names=FALSE)
