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



read_data = read.csv('/users/mejiaa/CAPSTONE/Data/df_log_and_scaled.csv')
data = do.call(data.frame,lapply(read_data, function(x) replace(x, is.infinite(x),0)))

data = data[,c(2:24)]
colnames(data)[23] <- "VAC_PAR"


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
random_forest_1 <- randomForest(VAC_PAR ~., data = train, ntree = 50, importance=TRUE, na.action = na.roughfix, maxnodes = 10)
preds_1 = predict(random_forest_1,test[-23])
parallelStop()

preds_1_cut = ifelse(preds_1>.5,1,0)
confusionMatrix(as.factor(preds_1_cut),as.factor(test$VAC_PAR), positive = "1")
F_meas(as.factor(preds_1_cut),as.factor(test$VAC_PAR))
