library(forestry)
library(ranger)
library(glmnet)
library(ggplot2)
library(xgboost)
source("src/gb_cv.R")
source("src/gb_forestry.R")
source("src/gb_part.R")

# Define all estimators:

estimator_grid <- list(
  "xgboost_1" = function(Xobs, Yobs)
    xgboost(data = data.matrix(Xobs), label= Yobs, nrounds = 20),
  "gb_forestry" = function(Xobs, Yobs)
    gradient_boosting_forestry(Xobs, Yobs, n_iterations = 20, eta = 0.3)
)



predictor_grid <- list(
  "xgboost_1" = function(estimator, feat) {
    feat <- data.matrix(feat)
    return(predict(estimator, feat))
  },
  "gb_forestry" = function(estimator, feat) {
    return(predict(estimator, feat))
  }
)
