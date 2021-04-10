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



read_data = read.csv('/users/mejiaa/CAPSTONE/Data/df_log_and_scaled.csv')
data = do.call(data.frame,lapply(read_data, function(x) replace(x, is.infinite(x),0)))

data = data[,c(2:24)]
colnames(data)[23] <- "VAC_PAR"


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

data_train = as.matrix(train[,c(1:22)])
data_train_l = as.matrix(train[,c(23)])
dtrain = xgb.DMatrix(data = data_train, label = data_train_l)

data_test = as.matrix(test[,c(1:22)])
data_test_l = as.matrix(test[,c(23)])
dtest = xgb.DMatrix(data = data_test, label = data_test_l  )

rand_seed = 959
params = list(booster = "gbtree", objective = "binary:logistic", eta = 0.1, max_depth = 7, min_child_weight=5.26, subsample=0.653, colsample_bytree=0.77)
xgboost_best = xgb.train(params = params, data = dtrain, nrounds = 100, watchlist = list(val = dtest, train = dtrain), print_every_n = 10, early_stopping_rounds = 10, maximize = F, eval_metric = "error")

xgbpred_best = predict(xgboost_best,dtest)
xgbpred_best_cut =  ifelse(xgbpred_best > 0.50,1,0)

confusionMatrix(as.factor(xgbpred_best_cut), as.factor(data_test_l), positive = "1")
F_meas(as.factor(xgbpred_best_cut),as.factor(data_test_l))

xgb.importance(feature_names = colnames(test[,c(1:22)]), model = xgboost_best, data=test[,c(1:22)], label=test[,23])

xgb.plot.importance(xgb.importance(feature_names = colnames(test[,c(1:22)]), model = xgboost_best, data=test[,c(1:22)], label=test[,23]), top_n = 12)

#generage ROC curve
myPred = prediction(xgbpred_best,test[,23])
perf = ROCR::performance(myPred,"tpr","fpr")
#calculate AUC
auc = ROCR::performance(myPred, measure="auc")
auc_score = auc@y.values[[1]]

#plot the curve
plot(perf,main=paste0("XGBoost ROC curve: AUC= ",auc_score), xlim=c(0,0.95), ylim=c(.55,1),colorize=TRUE)

