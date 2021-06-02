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


#read_data = read.csv('/users/mejiaa/CAPSTONE/Data/df_log_and_scaled.csv')
read_data = read.csv('https://raw.githubusercontent.com/andrewmejia600/Capstone/main/Data/df_log_and_scaled.csv')
#data = do.call(data.frame,lapply(read_data, function(x) replace(x, is.infinite(x),0)))

data = read_data
colnames(data)[1] = "VAC_PAR"

#Simpler Model
#data = data[,c(1,3,6,7,8,9)]


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

rand_seed = 42
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


##### Create best tune using all features 
train["VAC_PAR"] = as.character(ifelse(train["VAC_PAR"]==1,"T", "F"))
test["VAC_PAR"] = as.character(ifelse(test["VAC_PAR"]==1, "T", "F"))
traintask <- makeClassifTask (data = train,target = "VAC_PAR")
testtask <- makeClassifTask (data = test,target = "VAC_PAR")

lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="binary:logistic", eval_metric="error", nrounds=100L, eta=0.1)

params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree")), makeIntegerParam("max_depth",lower = 3L,upper = 10L), makeNumericParam("min_child_weight",lower = 1L,upper = 10L), makeNumericParam("subsample",lower = 0.5,upper = 1), makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

rdesc <- makeResampleDesc("CV",stratify = T,iters=10L)



ctrl <- makeTuneControlRandom(maxit = 10L)

parallelStartSocket(cpus=detectCores())
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, measures = acc, par.set = params, control = ctrl, show.info = T)

mytune$y

lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

xgmodel <- train(learner = lrn_tune,task = traintask)

xgpred = predict(xgmodel, testtask)
parallelStop()


confusionMatrix(xgpred$data$response,xgpred$data$truth, positive = "T")

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
print("Number of records in Training data")
nrow(train)
print("Number of records in Testing data")
nrow(test)

####### Train Features 
data_train = as.matrix(train[,c(2:6)])
####### Train Target Labels
data_train_l = as.matrix(train[,c(1)])
dtrain = xgb.DMatrix(data = data_train, label = data_train_l)

###### Test Features
data_test = as.matrix(test[,c(2:6)])
###### Test Target Labels
data_test_l = as.matrix(test[,c(1)])
dtest = xgb.DMatrix(data = data_test, label = data_test_l  )

rand_seed = 959
params = list(booster = "gbtree", objective = "binary:logistic", eta = 0.1, max_depth = 7, min_child_weight=5.26, subsample=0.653, colsample_bytree=0.77)
xgboost_best = xgb.train(params = params, data = dtrain, nrounds = 100, watchlist = list(val = dtest, train = dtrain), print_every_n = 10, early_stopping_rounds = 10, maximize = F, eval_metric = "error")

xgbpred_best = predict(xgboost_best,dtest)
xgbpred_best_cut =  ifelse(xgbpred_best > 0.50,1,0)

confusionMatrix(as.factor(xgbpred_best_cut), as.factor(data_test_l), positive = "1")
F_meas(as.factor(xgbpred_best_cut),as.factor(data_test_l))

xgb.importance(feature_names = colnames(test[,c(2:6)]), model = xgboost_best, data=test[,c(2:6)], label=test[,1])

xgb.plot.importance(xgb.importance(feature_names = colnames(test[,c(2:6)]), model = xgboost_best, data=test[,c(2:6)], label=test[,1]), top_n = 12)


#generate ROC curve
myPred = prediction(xgbpred_best,test[,1])



perf = ROCR::performance(myPred,"tpr","fpr")
#calculate AUC
auc = ROCR::performance(myPred, measure="auc")
auc_score = auc@y.values[[1]]

#plot the curve
plot(perf,main=paste0("XGBoost ROC curve: AUC= ",auc_score), xlim=c(0,0.95), ylim=c(.55,1),colorize=TRUE)


test['Tuned_XG_Preds'] = xgbpred_best
test['Tuned_XG_preds_Cut'] = xgbpred_best_cut


write.csv(test, 'XG_test_out.csv')

saveRDS(xgboost_best, "./final_model.rds")