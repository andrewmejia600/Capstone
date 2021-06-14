if (!require(caret)) install.packages('caret')
library(caret)

if (!require(parallel)) install.packages('parallel')
library(parallel)

if (!require(e1071)) install.packages('e1071')
library(e1071)

if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

if (!require(ROCR)) install.packages('ROCR')
library(ROCR)

if (!require(mlr)) install.packages('mlr')
library(mlr)

if (!require(caret)) install.packages('caret')
library(caret)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)



read_data_RF = read.csv('https://raw.githubusercontent.com/andrewmejia600/Capstone/main/Code/Models/RF_Model/RF_test_out.csv')

read_data_XG = read.csv('https://raw.githubusercontent.com/andrewmejia600/Capstone/main/Code/Models/XG_Model/XG_test_out.csv')

read_data_XG_Full = read.csv('https://raw.githubusercontent.com/andrewmejia600/Capstone/main/Code/Models/XG_Model_Full/XG_test_out.csv')

####################################RF 
read_data_RF['PAR_LABEL_R'] = factor(ifelse(read_data_RF$VAC_PAR == 1, "VAC", "NVAC"))
read_data_RF['PAR_LABEL_P'] = factor(ifelse(read_data_RF$Tuned_RF_preds_Cut == 1, "VAC", "NVAC"))



table_rf <- data.frame(confusionMatrix(read_data_RF$PAR_LABEL_P, read_data_RF$PAR_LABEL_R)$table)

plotTable_rf <- table_rf %>%
  mutate(outcome = ifelse(table_rf$Prediction == table_rf$Reference, "Good", "Bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))


p = ggplot(data = plotTable_rf, mapping = aes(x = Reference, y = Prediction, fill = outcome)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(Good = "#999999", Bad = "#FF9900")) +
  theme_minimal() +
  xlim(rev(levels(table_rf$Reference)))

p + ggtitle("Random Forest Confusion Matrix")

############################################################ XG Boost 

read_data_XG['PAR_LABEL_R'] = factor(ifelse(read_data_XG$VAC_PAR == 1, "VAC", "NVAC"))
read_data_XG['PAR_LABEL_P'] = factor(ifelse(read_data_XG$Tuned_XG_preds_Cut == 1, "VAC", "NVAC"))

table_XG <- data.frame(confusionMatrix(read_data_XG$PAR_LABEL_P, read_data_XG$PAR_LABEL_R)$table)

plotTable_XG = table_XG %>%
  mutate(outcome = ifelse(table_XG$Prediction == table_XG$Reference, "Good", "Bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))


p = ggplot(data = plotTable_XG, mapping = aes(x = Reference, y = Prediction, fill = outcome)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(Good = "#999999", Bad = "#FF9900")) +
  theme_minimal() +
  xlim(rev(levels(table_XG$Reference)))

p + ggtitle("XG-BOOST Confusion Matrix")


############################################################ XG Boost 

read_data_XG_Full['PAR_LABEL_R'] = factor(ifelse(read_data_XG_Full$VAC_PAR == 1, "VAC", "NVAC"))
read_data_XG_Full['PAR_LABEL_P'] = factor(ifelse(read_data_XG_Full$Tuned_XG_preds_Cut == 1, "VAC", "NVAC"))

table_XG <- data.frame(confusionMatrix(read_data_XG_Full$PAR_LABEL_P, read_data_XG_Full$PAR_LABEL_R)$table)

plotTable_XG = table_XG %>%
  mutate(outcome = ifelse(table_XG$Prediction == table_XG$Reference, "Good", "Bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))


p = ggplot(data = plotTable_XG, mapping = aes(x = Reference, y = Prediction, fill = outcome)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(Good = "#999999", Bad = "#FF9900")) +
  theme_minimal() +
  xlim(rev(levels(table_XG$Reference)))

p + ggtitle("XG-BOOST All Features Confusion Matrix")
 

