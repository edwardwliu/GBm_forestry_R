library(forestry)
library(gbm)
library("microbenchmark")
source("src/gb_forestry.R")

# Commonly used libraries for benchmarking
# 1. Sys.time() -> tictoc
# 2. system.time() -> rbenchmark vs. microbenchmark

x_values <- iris[,-c(1, 5)]
y_values <- iris[,1]

microbenchmark(gradient_boosting_forestry(
                x_values, y_values, ntree=10, n_iterations=10), times=10)
microbenchmark(multilayerForestry(
                x_values, y_values, ntree=10, nrounds=10, maxDepth=6), times=10)
microbenchmark(forestry(
                x_values, y_values, ntree=10, maxDepth=6), times=10)
  


