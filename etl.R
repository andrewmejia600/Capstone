# This is the ETL to create a master data set

# Read in files
path = "Data/"
vacant_parcels <- read.csv(paste0(path, "CPAL/DCAD_parcels_vac.csv"), stringsAsFactors = F)
account_apprl_year <- read.csv(paste0(path, "DCAD2019/account_apprl_year_small.csv"), stringsAsFactors = F)
account_info <- read.csv(paste0(path, "DCAD2019/account_info_small.csv"), stringsAsFactors = F)
account_tif <- read.csv(paste0(path, "DCAD2019/account_tif_small.csv"), stringsAsFactors = F)
account_exempt_value <- read.csv(paste0(path, "DCAD2019/acct_exempt_value_small.csv"), stringsAsFactors = F)
account_exempt_value$ACCOUNT_NUM <- as.character(account_exempt_value$ACCOUNT_NUM)
applied_std_exempt <- read.csv(paste0(path, "DCAD2019/applied_std_exempt_small.csv"), stringsAsFactors = F)
com_detail <- read.csv(paste0(path, "DCAD2019/com_detail_small.csv"), stringsAsFactors = F)
land <- read.csv(paste0(path, "DCAD2019/land_small.csv"), stringsAsFactors = F)
land$ACCOUNT_NUM <- as.character(land$ACCOUNT_NUM)
multi_owner <- read.csv(paste0(path, "DCAD2019/multi_owner_small.csv"), stringsAsFactors = F)
res_addl <- read.csv(paste0(path, "DCAD2019/res_addl_small.csv"), stringsAsFactors = F)
res_detail <- read.csv(paste0(path, "DCAD2019/res_detail_small.csv"), stringsAsFactors = F)
taxable_object <- read.csv(paste0(path, "DCAD2019/taxable_object_small.csv"), stringsAsFactors = F)
taxable_object$ACCOUNT_NUM <- as.character(taxable_object$ACCOUNT_NUM)
taxable_object$TAX_OBJ_ID <- as.character(taxable_object$TAX_OBJ_ID)

# Join all the files together
df = vacant_parcels %>%
  left_join(account_apprl_year) %>%
  left_join(account_info) %>%
  left_join(account_tif) %>%
  left_join(account_exempt_value) %>%
  left_join(applied_std_exempt) %>%
  left_join(com_detail) %>%
  left_join(land) %>%
  left_join(multi_owner) %>%
  left_join(res_addl) %>%
  left_join(res_detail) %>%
  left_join(taxable_object)

saveRDS(df, "master_data.rds")