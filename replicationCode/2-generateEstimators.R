library(forestry)
library(ranger)
library(glmnet)
library(ggplot2)
library(xgboost)
source("src/gradient_boosting.R")
source("src/gb_forestry.R")

# Define all estimators:

estimator_grid <- list(
  "ranger_1" = function(Xobs, Yobs)
    ranger(Yobs ~., data = cbind(Xobs, Yobs)),
  "ranger_2" = function(Xobs, Yobs)
    ranger(Yobs ~., data = cbind(Xobs, Yobs)),
  "ranger_3" = function(Xobs, Yobs)
    ranger(Yobs ~., data = cbind(Xobs, Yobs)),

  "glmnet_1" = function(Xobs, Yobs)
    glmnet(x = data.matrix(Xobs), y = Yobs, alpha = 1),
  "glmnet_2" = function(Xobs, Yobs)
    glmnet(x = data.matrix(Xobs), y = Yobs, alpha = 0),
  "glmnet_3" = function(Xobs, Yobs)
    glmnet(x = data.matrix(Xobs), y = Yobs, alpha = .5),
  
  "xgboost_1" = function(Xobs, Yobs)
    xgboost(data = data.matrix(Xobs), label= Yobs, nrounds = 10),
  "gb_rpart_1" = function(Xobs, Yobs)
    gradient_boosting(Xobs, Yobs, n_iterations = 10),
  "gb_forestry_1" = function(Xobs, Yobs)
    gradient_boosting_forestry(Xobs, Yobs, n_iterations = 10)
)



predictor_grid <- list(
  "ranger_1" = function(estimator, feat) {
    return(predict(estimator, feat)$predictions)
  },
  "ranger_2" = function(estimator, feat) {
    return(predict(estimator, feat)$predictions)
  },
  "ranger_3" = function(estimator, feat) {
    return(predict(estimator, feat)$predictions)
  },

  "glmnet_1" = function(estimator, feat) {
    feat <- data.matrix(feat)
    l <- estimator$lambda[estimator$lambda == min(estimator$lambda)]
    return(predict(estimator, s = l, newx = feat))
  },
  "glmnet_2" = function(estimator, feat) {
    feat <- data.matrix(feat)
    l <- estimator$lambda[estimator$lambda == min(estimator$lambda)]
    return(predict(estimator, s = l, newx = feat))
  },
  "glmnet_3" = function(estimator, feat) {
    feat <- data.matrix(feat)
    l <- estimator$lambda[estimator$lambda == min(estimator$lambda)]
    return(predict(estimator, s = l, newx = feat))
  },

  "xgboost_1" = function(estimator, feat) {
    feat <- data.matrix(feat)
    return(predict(estimator, feat))
  },
  "gb_rpart_1" = function(estimator, feat) {
    return(predict(estimator, feat))
  },
  "gb_forestry_1" = function(estimator, feat) {
    return(predict(estimator, feat))
  }
)
