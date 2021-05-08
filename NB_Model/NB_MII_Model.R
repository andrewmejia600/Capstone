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

if (!require(naivebayes)) install.packages('naivebayes')
library(naivebayes)

if (!require(e1071)) install.packages(e1071)
library(e1071)



#read_data = read.csv('/users/mejiaa/CAPSTONE/Data/df_log_and_scaled.csv')
read_data = read.csv('https://raw.githubusercontent.com/andrewmejia600/Capstone/main/Data/df_log_and_scaled.csv')

data = do.call(data.frame,lapply(read_data, function(x) replace(x, is.infinite(x),0)))

data = data[,c(2:24)]
colnames(data)[23] = "VAC_PAR"
data$VAC_PAR = as.factor(data$VAC_PAR) 


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
# create baseline nb model
parallelStartSocket(cpus=detectCores())
nb_model =  naive_bayes(VAC_PAR ~., data = train, usekernel = T) 
preds_1 = predict(nb_model,test[,-23],)


parallelStop()


cm = table(preds_1, test$VAC_PAR)
cm

# Model Evaluation
confusionMatrix(cm)
F_meas(preds_1, test$VAC_PAR)
