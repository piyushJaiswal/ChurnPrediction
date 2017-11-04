
rm(list = ls())
gc()



library(data.table)
library(xgboost)
library(dplyr)



setwd("~/Documents/StudyWork/AV/Churn/CODES")
#source("lib.R")



testing = T



train <- fread("../DATA/train.csv")
colnames(train)[-1] <- tolower(colnames(train)[-1])
print(dim(train))
gc()



## Imputations ----------------------------------
train$zip = as.integer(train$zip)



## Convert date columns --------------------------------



## Remove columns with just 1 value-----------------------------
cols_train_orig = c(colnames(train))
for(c in cols_train_orig){
  if(length(unique(train[,get(c)]))==1){
    print(c)
    train[,(c):=NULL]
  }
}



## Remove Y/N columns where proportion of Y is less than 1 in 30000-------------------------
cols_train_orig = c(colnames(train))
for(c in cols_train_orig){
  
  if((length(unique(train[,get(c)]))==2) & (class(train[,get(c)])=="character")){
    x = sort(table(train[,get(c)]))
    
    if(x[1]<10){
      
      print(c)
      print(unique(train[,get(c)]))
      train[,(c):=NULL]
      
    }
    
  }
}



if(!testing){
  write.csv(train, file = "../DERIVED/train.csv", row.names = F)
}



## get final list of features - cols_select --------------------------
cols_reject = c("UCIC_ID", "responders")
cols_select= colnames(train)[!(colnames(train) %in% cols_reject)]



## separate independent and dependent variables -------------------------
x_train <- subset(train, select = cols_select)
y_train <- train$responders
rm(train)
gc()



## create a 70-30 train-validation split -------------------------------------------------------------------
set.seed(12345)
if(testing){
  indx <- sample(1:nrow(x_train), 0.5*nrow(x_train), replace = F)
  x_train <- x_train[-indx,]
  y_train <- y_train[-indx]
}
set.seed(12345)
indx <- sample(1:nrow(x_train), 0.3*nrow(x_train), replace = F)
x_test <- x_train[indx,]
y_test <- y_train[indx]

x_train <- x_train[-indx,]
y_train <- y_train[-indx]
gc()



# Numeric encoding for the factor variables
cols_fac_lev <- list()
cols_fac <- colnames(x_train)[sapply(x_train, class)=="character" | sapply(x_train, class)=="factor"]
for(c in cols_fac){
  
  #print(c)
  
  levels_fac <- unique(x_train[, get(c)])
  levels_fac = sort(levels_fac)
  
  x_train[, (c) := as.integer(factor(get(c), levels = levels_fac))]
  x_test[, (c) := as.integer(factor(get(c), levels = levels_fac))]
  
  if("Others" %in% levels_fac){
    x_test[is.na(get(c)), (c) := "Others"]
  }
  
  print(length(which(is.na(x_test[, get(c)]))))
  
  cols_fac_lev[[c]] = levels_fac
  
}



## prepare data for xgboost - xgb.Dmatrix for train and validation -------------------------------------------------------------------
train.xg <- xgb.DMatrix(as.matrix(x_train), label=y_train, missing=NA)
test.xg <- xgb.DMatrix(as.matrix(x_test),label=y_test, missing=NA)
rm(x_train, x_test)
gc()



## Model parameters -------------------------------------------------------------------
log_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
if(testing){
  fname_msgs = paste0("../LOGS/msgs_exp_", log_time, ".txt")
}else{
  fname_msgs = paste0("../LOGS/msgs_", log_time, ".txt")
}

#fname_msgs = paste0("../LOGS/msgs_", log_time, ".txt")
file_msgs <- file(fname_msgs, open="wt")
sink(file_msgs, type="output")
params <- list(
  "objective"           = "binary:logistic",
  "eval_metric"         = "auc",
  "eta"                 = 0.01,
  "max_depth"           = 10,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.76,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)
print(params)
print("\n")



## Train Model with xgb.cv for optimal nrounds and then with xgb.train-------------------------------------------------------------------
Sys.time()
set.seed(123)
if(testing){
  numrounds = 1001
}else{
  numrounds = 1501
}
model_xgb <- xgb.train(data=train.xg, nrounds = numrounds,
                       params = params, verbose = 1, missing = NA, 
                       early.stop.round = 200, 
                       maximize = T, print.every.n = 100,
                       watchlist = list(validation1 = test.xg, validation2 = train.xg)
)
Sys.time()
sink(file = NULL, type = "output")
closeAllConnections()



importance <- data.frame(xgb.importance(cols_select, model = model_xgb))



save(importance, file = "../MODELS/model_xgb_imp_1.Rdata")
save(model_xgb, file = "../MODELS/model_xgb_1.Rdata")
gc()




## Start scoring ----------------------------------------------
test <- fread("../DATA/test.csv")
colnames(test)[-1] <- tolower(colnames(test)[-1])



## Imputations ----------------------------------
test$zip = as.integer(test$zip)



## Convert date columns --------------------------------



write.csv(test, file = "../DERIVED/test.csv", row.names = F)



## select columns ----------------------------------------------
x_score <- subset(test, select = cols_select)



# numeric encoding ----------------------------------------------------
cols_fac <- names(cols_fac_lev)
for(c in cols_fac){
  
  levels_fac <- cols_fac_lev[[c]]
  
  x_score[, (c) := as.integer(factor(get(c), levels = levels_fac))]
  
  if("Others" %in% levels_fac){
    x_score[is.na(get(c)), (c) := "Others"]
  }
  
}



## prepare data for xgboost - xgb.Dmatrix ------------------------------------------------------
score.xg <- xgb.DMatrix(as.matrix(x_score), missing = NA)
gc()



## Score data-------------------------------------------------------------------
Sys.time()
preds_score = predict(model_xgb, score.xg, ntreelimit = model_xgb$bestIteration, missing = NA)
test[, Responders := preds_score]



# make submission
submission <- subset(test, select = c("UCIC_ID", "Responders"))
write.csv(submission, file = "../SUBMISSION/submission_xgb_1.csv", row.names = F)


