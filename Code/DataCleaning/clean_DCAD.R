

########################
# Read, combine and prepare data from Dallas Central Appraisal District (DCAD)

# account_apprl_year.csv
# land.csv
########################


library(dplyr)
library(tidyverse)
#file_direct = "C:/SMU_Local/data/capstone/Data/DCAD2019_CURRENT/"
file_direct = "/Users/tina/Documents/School/Capstone/Dallas Files/DCAD2019_CERTIFIED_07252019/"
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
                                                                    ifelse(str_detect(ZONING, "LIQUOR"), "liquor","other"))))))))

summary(land)

# New Feature:  AREA_SQFT
# Calculate total area of parcel in square feet.  Some parcels are measured in acres.

l=as.numeric(length(land$AREA_SIZE))
i=0
land$AREA_SQFT=0
land$AREA_UOM_DESC=as.character(land$AREA_UOM_DESC)
for(i in 1:l){
  if (land$AREA_UOM_DESC[i] == 'ACRE')
  {land$AREA_SQFT[i] = land$AREA_SIZE[i]*43560
  }
  else if (land$AREA_UOM_DESC[i] == 'UNASSIGNED'){
    land$AREA_SQFT[i] = land$FRONT_DIM[i]*land$DEPTH_DIM[i]
  }
  else
  {land$AREA_SQFT[i] = land$AREA_SIZE[i]
  }
}

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

df_apprl$num_div_code = ifelse(df_apprl$division_cd == 'RES', 1, 
       ifelse(df_apprl$division_cd == 'COM', 2, 3))

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

x=df_apprl$sptd_code

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


barplot(table(df_apprl$num_sptd))




##################### Join land and apprl datasets to create df_DCAD


df_DCAD = left_join(df_apprl,df_land, by = c("Acct" = "Acct"), keep = FALSE)

# Log (base e) Normalize value factors and area_sqft - range of values is extreme.

df_DCAD$log_impr_val = log(df_DCAD$impr_val)
df_DCAD$log_land_val = log(df_DCAD$land_val)
df_DCAD$log_tot_val = log(df_DCAD$tot_val)
df_DCAD$log_area_sqft = log(df_DCAD$area_sqft)

# Note:  Values include parcels outside of City of Dallas.
# DCAD continuous factors will be scaled after left join with df in create_dataset.r

write.csv(df_DCAD, '../../Data/clean_DCAD.csv',quote = TRUE)
