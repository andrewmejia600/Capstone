if (!require(randomForest)) install.packages('randomForest')
library(randomForest)
if (!require(caret)) install.packages('caret')
library(caret)
if (!require(ROCR)) install.packages('ROCR')
library(ROCR)

if (!require(mlr)) install.packages('mlr')
library(mlr)

if (!require(parallelMap)) install.packages('parallelMap')
library(parallelMap)

if (!require(parallel)) install.packages('parallel')
library(parallel)



#read_data = read.csv('/users/mejiaa/CAPSTONE/Data/df_log_and_scaled.csv')
read_data = read.csv('https://raw.githubusercontent.com/andrewmejia600/Capstone/main/Data/df_log_and_scaled.csv')
#data = do.call(data.frame,lapply(read_data, function(x) replace(x, is.infinite(x),0)))

data = read_data
colnames(data)[1] = "VAC_PAR"


#Simpler model features. 
data = data[,c(1,3,6,7,8,9)]

#######################################################################################################################
# read_data = read.csv('https://raw.githubusercontent.com/andrewmejia600/Capstone/Andrew/Data/df_log_and_scaled.csv')##
# 
# colnames(read_data)[1] = "VAC_PAR"                                                                                 ##
# 
# data_1 = read_data                                                                                                 ##  
# 
# data = read_data                                                                                                   ##
# 
# dmy = dummyVars(~division_cd, data = data, fullRank = T)                                                           ##
# dat_transformed = data.frame(predict(dmy, newdata = data_1))                                                       ##
# data = cbind(data, dat_transformed)                                                                                ##
# 
# 
# dmy = dummyVars(~sptd_code, data = data, fullRank = T)                                                             ##
# dat_transformed = data.frame(predict(dmy, newdata = data_1))                                                       ##
# data = cbind(data, dat_transformed)                                                                                ##  
# 
# dmy = dummyVars(~zoning_buckets, data = data, fullRank = T)                                                        ##
# dat_transformed = data.frame(predict(dmy, newdata = data_1))                                                       ##  
# data = cbind(data, dat_transformed)                                                                                ##
# 
# data = data[,c(-2,-3,-5)]                                                                                          ##

#######################################################################################################################






rand_seed = 959
set.seed(rand_seed)
train_partition =  createDataPartition(
  y= data$VAC_PAR,
  p = .70,
  list = FALSE
)
train = data[train_partition,]
test =  data[-train_partition,]
print("Number of records in Training data")
nrow(train)
print("Number of records in Testing data")
nrow(test)

rand_seed = 959
set.seed(rand_seed)
# create baseline random forest model
parallelStartSocket(cpus=detectCores())
random_forest_1 <- randomForest(VAC_PAR ~., data = train, ntree = 13, importance=TRUE, na.action = na.roughfix, maxnodes = 5)
preds_1 = predict(random_forest_1,test[,-1])
parallelStop()



preds_1_cut = ifelse(preds_1>.5,1,0)
confusionMatrix(as.factor(preds_1_cut),as.factor(test$VAC_PAR), positive = "1")
F_meas(as.factor(preds_1_cut),as.factor(test$VAC_PAR))
set.seed(rand_seed)
varImpPlot(random_forest_1, cex = .7, main = "Variable Importance",pt.cex = 1,color = 'grey41',frame.plot = FALSE,lcolor = 'black')


rand_seed = 959
set.seed(rand_seed)
# create tuned random forest model
parallelStartSocket(cpus=detectCores())
random_forest_1_best <- randomForest(VAC_PAR ~., data = train, ntree = 13, importance=TRUE, na.action = na.roughfix, maxnodes = 5, mtry = 3)

preds_1_best = predict(random_forest_1_best,test[,-1])

parallelStop()

preds_1_cut_best = ifelse(preds_1_best>.5,1,0)
confusionMatrix(as.factor(preds_1_cut_best),as.factor(test$VAC_PAR), positive = "1")
F_meas(as.factor(preds_1_cut_best),as.factor(test$VAC_PAR))
set.seed(rand_seed)
varImpPlot(random_forest_1, cex = .7, main = "Variable Importance",pt.cex = 1,color = 'grey41',frame.plot = FALSE,lcolor = 'black')

test['BaseLine_RF_Preds'] = preds_1
test['BaseLine_RF_preds_Cut'] = preds_1_cut
test['Tuned_RF_Preds'] = preds_1_best
test['Tuned_RF_preds_Cut'] = preds_1_cut_best

#generate ROC curve
myPred = prediction(preds_1_best,test[,1])



perf = ROCR::performance(myPred,"tpr","fpr")
#calculate AUC
auc = ROCR::performance(myPred, measure="auc")
auc_score = auc@y.values[[1]]

#plot the curve
plot(perf,main=paste0("RF ROC curve: AUC= ",auc_score), xlim=c(0,0.95), ylim=c(.55,1),colorize=TRUE)

write.csv(test, 'RF_test_out.csv')

saveRDS(random_forest_1_best, "./RF_final_model.rds")
