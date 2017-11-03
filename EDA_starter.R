
rm(list = ls())
gc()



library(data.table)
library(xgboost)
library(dplyr)



setwd("~/Documents/StudyWork/AV/Churn/CODES")



testing = F



train <- fread("../DATA/train.csv")
print(dim(train))
gc()



## CUSTOMER Profile -------------------------------------------
summary(train$NO_OF_Accs)
length(unique(train$HNW_CATEGORY)); sort(table(train$HNW_CATEGORY), decreasing = T)
summary(train$vintage)
length(unique(train$EMAIL_UNSUBSCRIBE)); sort(table(train$EMAIL_UNSUBSCRIBE), decreasing = T)
length(unique(train$OCCUP_ALL_NEW)); sort(table(train$OCCUP_ALL_NEW), decreasing = T)
length(unique(train$city)); sort(table(train$city), decreasing = T)[1:100]
length(unique(train$zip)); summary(train$zip)
length(unique(train$dependents)); sort(table(train$dependents), decreasing = T); summary(train$dependents)
unlist(lapply(1:9, function(x) any(is.na(train[,x,with=F]))))
unlist(lapply(1:9, function(x) any(train[,x,with=F]=="")))



## MONTHLY TRANSACTION Details ----------------------------------------




## MONTHLY Bank details --------------------------------------------










