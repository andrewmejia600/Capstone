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

if (!require(parallelMap)) install.packages('parallelMap')
library(parallelMap)

if (!require(xgboost)) install.packages('xgboost')
library(xgboost)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

if (!require(aod)) install.packages('aod')
library(aod)



read_data = read.csv('https://raw.githubusercontent.com/andrewmejia600/Capstone/main/Data/df_log_and_scaled.csv')
#data = do.call(data.frame,lapply(read_data, function(x) replace(x, is.infinite(x),0)))

data = read_data
colnames(data)[1] = "VAC_PAR"

#Test Crime and CO and 311 Significance
data = data[,c(1,3,6,7,8,9, 10, 12,21)]


#### Generate new seed for test train split for actual model from best tune 
rand_seed = 959
set.seed(rand_seed)
train_partition =  createDataPartition(
  y= data$VAC_PAR,
  p = .70,
  list = FALSE
)
train = data[train_partition,]
train_ds = downSample(x = train[, -c(1)],
                                 y = as.factor(train[,1]))
colnames(train_ds)[9] = 'VAC_PAR'

train = train_ds
test =  data[-train_partition,]

glm = glm(formula = VAC_PAR ~ ., data = train, family = binomial(link='logit'))

summary(glm)


wald.test(b = coef(glm), Sigma = vcov(glm), Terms = 7)
wald.test(b = coef(glm), Sigma = vcov(glm), Terms = 8)
wald.test(b = coef(glm), Sigma = vcov(glm), Terms = 9)
wald.test(b = coef(glm), Sigma = vcov(glm), Terms = 2:9)
wald.test(b = coef(glm), Sigma = vcov(glm), Terms = 7:9)

fitted.results = predict(glm,newdata=test,type='response')
fitted.results = ifelse(fitted.results > 0.5,1,0)

confusionMatrix(as.factor(fitted.results),as.factor(test$VAC_PAR), positive = "1")
F_meas(as.factor(fitted.results),as.factor(test$VAC_PAR))

test['Preds'] = fitted.results

#generate ROC curve
myPred = prediction(fitted.results,test[,1])



perf = ROCR::performance(myPred,"tpr","fpr")
#calculate AUC
auc = ROCR::performance(myPred, measure="auc")
auc_score = auc@y.values[[1]]

#plot the curve
plot(perf,main=paste0("GLM ROC curve: AUC= ",auc_score), xlim=c(0,0.95), ylim=c(.55,1),colorize=TRUE)


############################################################ All CO and Crime Features CF

test['PAR_LABEL_R'] = factor(ifelse(test$VAC_PAR == 1, "VAC", "NVAC"))
test['PAR_LABEL_P'] = factor(ifelse(test$Preds == 1, "VAC", "NVAC"))

table_XG <- data.frame(confusionMatrix(test$PAR_LABEL_P, test$PAR_LABEL_R)$table)

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

p + ggtitle("GLM Confusion Matrix")




############################################################### Best RF features compare
data = read_data
colnames(data)[1] = "VAC_PAR"

#Simpler model features. 
data = data[,c(1,3,6,7,8,9)]


#### Generate new seed for test train split for actual model from best tune 
rand_seed = 959
set.seed(rand_seed)
train_partition =  createDataPartition(
  y= data$VAC_PAR,
  p = .70,
  list = FALSE
)
train = data[train_partition,]
train_ds = downSample(x = train[, -c(1)],
                      y = as.factor(train[,1]))
colnames(train_ds)[6] = 'VAC_PAR'

train = train_ds

test =  data[-train_partition,]

glm_s = glm(formula = VAC_PAR ~ ., data = train, family = binomial(link='logit'))

summary(glm_s)


wald.test(b = coef(glm_s), Sigma = vcov(glm_s), Terms = 6)

wald.test(b = coef(glm_s), Sigma = vcov(glm_s), Terms = 2:6)

fitted.results = predict(glm_s,newdata=test,type='response')
fitted.results = ifelse(fitted.results > 0.5,1,0)

confusionMatrix(as.factor(fitted.results),as.factor(test$VAC_PAR), positive = "1")
F_meas(as.factor(fitted.results),as.factor(test$VAC_PAR))

test['Preds'] = fitted.results

#generate ROC curve
myPred = prediction(fitted.results,test[,1])



perf = ROCR::performance(myPred,"tpr","fpr")
#calculate AUC
auc = ROCR::performance(myPred, measure="auc")
auc_score = auc@y.values[[1]]

#plot the curve
plot(perf,main=paste0("GLM ROC curve: AUC= ",auc_score), xlim=c(0,0.95), ylim=c(.55,1),colorize=TRUE)

############################################################ All CO and Crime Features CF

test['PAR_LABEL_R'] = factor(ifelse(test$VAC_PAR == 1, "VAC", "NVAC"))
test['PAR_LABEL_P'] = factor(ifelse(test$Preds == 1, "VAC", "NVAC"))

table_XG <- data.frame(confusionMatrix(test$PAR_LABEL_P, test$PAR_LABEL_R)$table)

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

p + ggtitle("GLM Confusion Matrix")



save(glm_s, file = './glmmodel')
