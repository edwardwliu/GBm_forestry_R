set.seed(771251)

devtools::install_github("soerenkuenzel/forestry", ref = "multilayer-forestry")
library(forestry)
library(gbm)
library(tidyverse)
library("microbenchmark")
try(source("src/gb_forestry.R"), silent = TRUE)
try(source("~/Dropbox/GBm_forestry_R/src/gb_forestry.R"), silent = TRUE)

# Commonly used libraries for benchmarking
# 1. Sys.time() -> tictoc
# 2. system.time() -> rbenchmark vs. microbenchmark

x_values <- iris[,-c(1, 5)]
y_values <- iris[,1]

microbenchmark(
  gradient_boosting_forestry(x_values, y_values, ntree = 10, n_iterations = 10),
  times = 50, unit = "ms")
microbenchmark(
  multilayerForestry(x_values, y_values, ntree = 10, nrounds = 10, maxDepth = 6), 
  times = 50, unit = "ms")
microbenchmark(
  forestry(x_values, y_values, ntree = 10, maxDepth = 6), 
  times = 50, unit = "ms")

#TODO(Edward): 
# 1.) Could you add in gbm benchmark?
# 2.) gradient_boosting_forestry does not have a maxDepth yet, right? This is
# not too important for this speed benchmark, but it will be more important,
# when we look at the MSE.


# How does it scale per nrounds? -----------------------------------------------

nrounds_max <- 30

speed <- data.frame()

for (nr in 0:nrounds_max) {
  print(paste0("Running nrounds = ", nr))
  bm <- microbenchmark(
    multilayerForestry(x_values, y_values, ntree = 10, nrounds = nr, maxDepth = 6), 
    times = 50, unit = "ms")
  bms <- summary(bm)
  speed <- rbind(speed, 
                 data.frame(
                   nrounds = nr, 
                   mean = bms$mean))
}

speed

speed_forestry <- microbenchmark(
  forestry(x_values, y_values, ntree = 10, maxDepth = 6), 
  times = 50, unit = "ms")

speed %>% 
  ggplot(aes(x = nrounds, y = mean)) +
  geom_point() +
  geom_hline(yintercept = summary(speed_forestry)$mean, linetype = 2) +
  ylab("time in ms")

ggsave("replicationCode/006f-speed_benchmark.pdf", width = 8, height = 8)

#TODO(Edward): 
# 3.) What is the behavior for nrounds = 0 and why is nrounds = 1 not equal to 
# the performance of the default forestry?


