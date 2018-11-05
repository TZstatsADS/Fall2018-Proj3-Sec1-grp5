#########################################################
### Train a XGboost model with training features ###
#########################################################

### Author: Hengyang Lin
### Project 3

load("../output/feature_train.RData")
featdat <- dat_train$feature
labdat <- dat_train$label

#take one layer as an example
xdat <- featdat[,,1]
ydat <- labdat[,1,1]


XGB_train <- function(dat_train, label_train){
  library(xgboost)
  xg_mat <- xgb.DMatrix(data = xdat, label = ydat)
  #tuning
  params <- list(booster = "gbtree")
  cv.res <- xgb.cv(params = params, data = xg_mat, nfold = 5, nrounds = 20, objective = "reg:linear", early_stopping_rounds = 5)
}