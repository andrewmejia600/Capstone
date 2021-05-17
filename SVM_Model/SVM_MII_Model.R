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




parallelStartSocket(cpus=detectCores())
SVM_CLF = svm(formula = VAC_PAR~ .,
                  data = train,
                  type = 'C-classification',
                  kernel = 'radial')

SVM_Pred_out = predict(SVM_CLF, newdata = test[1:22])

parallelStop()

confusionMatrix(as.factor(SVM_Pred_out),as.factor(test$VAC_PAR), positive = "1")
F_meas(as.factor(SVM_Pred_out),as.factor(test$VAC_PAR))




