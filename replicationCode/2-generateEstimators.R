library(forestry)
library(ranger)
library(glmnet)
library(ggplot2)
library(xgboost)
source("src/gb_cv.R")
source("src/gb_forestry.R")
source("src/gb_rpart.R")

# Define all estimators:

estimator_grid <- list(
  "xgboost_rounds=10" = function(Xobs, Yobs)
    xgboost(data = data.matrix(Xobs), label = Yobs, nrounds = 10),
  "xgboost_rounds=10,gain=10" = function(Xobs, Yobs)
    xgboost(data = data.matrix(Xobs), label = Yobs, nrounds = 10, gamma = 10),
  "xgboost_rounds=50" = function(Xobs, Yobs)
    xgboost(data = data.matrix(Xobs), label = Yobs, nrounds = 50),
  "xgboost_rounds=50,gain=10" = function(Xobs, Yobs)
    xgboost(data = data.matrix(Xobs), label = Yobs, nrounds = 50, gamma = 10),
  "forestry" = function(Xobs, Yobs)
    forestry(x = Xobs, y = Yobs, ntree = 100, replace = FALSE, 
             sampsize = nrow(Xobs), mtry = ncol(Xobs), 
             nodesizeStrictSpl = max(round(nrow(Xobs)/128), 1), 
             nodesizeStrictAvg = max(round(nrow(Xobs)/128), 1), 
             splitrule = "variance", middleSplit = TRUE, maxDepth = 6)
  # "gbForestry_r_10x10" = function(Xobs, Yobs)
  #   gradient_boosting_forestry(Xobs, Yobs, ntree = 10, n_iterations = 10),
  # "multilayerForestry" = function(Xobs, Yobs)
  #   multilayerForestry(x = Xobs, y = Yobs, ntree = 10,
  #                      maxDepth = 6, nrounds = 10)
)


predictor_grid <- list(
  "xgboost_rounds=10" = function(estimator, feat) {
    feat <- data.matrix(feat)
    return(predict(estimator, feat))
  },
  "xgboost_rounds=10,gain=10" = function(estimator, feat) {
    feat <- data.matrix(feat)
    return(predict(estimator, feat))
  },
  "xgboost_rounds=50" = function(estimator, feat) {
    feat <- data.matrix(feat)
    return(predict(estimator, feat))
  },
  "xgboost_rounds=50,gain=10" = function(estimator, feat) {
    feat <- data.matrix(feat)
    return(predict(estimator, feat))
  },
  "forestry" = function(estimator, feat) {
    return(predict(estimator, feat))
  }
  # "gbForestry_r_10x10" = function(estimator, feat) {
  #   return(predict(estimator, feat))
  # },
)
