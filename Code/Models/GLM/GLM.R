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
test =  data[-train_partition,]

glm = glm(formula = VAC_PAR ~ ., data = train, family = binomial(link='logit'))

summary(glm)

anova(glm, test="Chisq")

fitted.results = predict(glm,newdata=test,type='response')
fitted.results = ifelse(fitted.results > 0.5,1,0)

confusionMatrix(as.factor(fitted.results),as.factor(test$VAC_PAR), positive = "1")
F_meas(as.factor(fitted.results),as.factor(test$VAC_PAR))

#generate ROC curve
myPred = prediction(fitted.results,test[,1])



perf = ROCR::performance(myPred,"tpr","fpr")
#calculate AUC
auc = ROCR::performance(myPred, measure="auc")
auc_score = auc@y.values[[1]]

#plot the curve
plot(perf,main=paste0("GLM ROC curve: AUC= ",auc_score), xlim=c(0,0.95), ylim=c(.55,1),colorize=TRUE)


############################################################### Best RF compare
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
test =  data[-train_partition,]

glm = glm(formula = VAC_PAR ~ ., data = train, family = binomial(link='logit'))

summary(glm)

anova(glm, test="Chisq")

fitted.results = predict(glm,newdata=test,type='response')
fitted.results = ifelse(fitted.results > 0.5,1,0)

confusionMatrix(as.factor(fitted.results),as.factor(test$VAC_PAR), positive = "1")
F_meas(as.factor(fitted.results),as.factor(test$VAC_PAR))

#generate ROC curve
myPred = prediction(fitted.results,test[,1])



perf = ROCR::performance(myPred,"tpr","fpr")
#calculate AUC
auc = ROCR::performance(myPred, measure="auc")
auc_score = auc@y.values[[1]]

#plot the curve
plot(perf,main=paste0("GLM ROC curve: AUC= ",auc_score), xlim=c(0,0.95), ylim=c(.55,1),colorize=TRUE)



save(glm, file = './glmmodel')
