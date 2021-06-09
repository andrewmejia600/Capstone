#
# Merge Acct back onto RF_test_out.csv to be able to check Acct values in the online map.
#
####################

library(dplyr)

file_direct = "/Users/tina/Documents/School/Capstone/Dallas Files/GIS_PACKAGE_FILES_TO_CSV/"
# data downloaded from 'https://raw.githubusercontent.com/andrewmejia600/Capstone/Andrew/XG_Model/XG_test_out.csv'
XG_test_out = read.csv(paste0(file_direct, "XG_test_out.csv"))
df_Acct = read.csv('../../Data/df_all_with_Acct_GIS.csv')

# Note:  RF_test_out rowname value stored in X
# df_Acct rowname value is original value for complete dataset
df_Acct$X = as.integer(rownames(df_Acct))
new_df = XG_test_out %>%
  select(X, Tuned_XG_Preds, Tuned_XG_preds_Cut) %>%
  left_join(df_Acct, by = "X")

write.csv(new_df, "../../Data/results_w_acct.csv")
