library(forestry)

gradient_boosting_forestry <- function(x_values, y_values, n_iterations, ntree = 1,
                                       eta = 0.3, splitratio = 1) {
  y_mean <- mean(y_values)
  y_predicted <- rep(y_mean, length(y_values))
  
  gamma_vec <- rep(NA, n_iterations)
  tree_list <- list()
  
  h_i <- function(tree_model, test_values) {
    prediction <- predict(tree_model, x_values)
    return(prediction)
  }
  
  for (i in 1:n_iterations) {
    # Get pseudo residuals -----------------------------------------------------
    residuals <- y_values - y_predicted
    
    # Train tree predictor -----------------------------------------------------
    tree_fit <- forestry(x = x_values, y = residuals, ntree = ntree, 
                         replace = FALSE, sampsize = nrow(x_values),
                         mtry = ncol(x_values), 
                         nodesizeStrictSpl = max(round(nrow(x_values)/128), 1), 
                         nodesizeStrictAvg = max(round(nrow(x_values)/128), 1), 
                         splitratio = splitratio, splitrule = "variance", 
                         middleSplit = TRUE, maxDepth = 6)
    tree_list[[i]] <- tree_fit
    
    # Find gamma_min -----------------------------------------------------------
    gamma_grid <- seq(from = 1, -1, length.out = 100)
    
    best_min <- Inf
    best_gamma <- NA
    
    for (gamma in gamma_grid) {
      fitted_values_gamma <- y_predicted + gamma * h_i(tree_fit)
      current_val <- mean((y_values - fitted_values_gamma) ^ 2)
      if (best_min > current_val) {
        best_min <- current_val
        best_gamma <- gamma
      }
    }
    gamma_vec[i] <- best_gamma * eta
    
    y_predicted <- rep(y_mean, length(y_values))
    for (j in 1:i) {
      y_predicted <- y_predicted + gamma_vec[j] * h_i(tree_list[[j]])
    }
  }
  
  # Return model ---------------------------------------------------------------
  model <- list(
    mean = y_mean,
    gamma = gamma_vec,
    tree = tree_list,
    iter = n_iterations
  )
  class(model) <- 'gb_forestry'
  return(model)
}

predict.gb_forestry <- function(model_object, x_values) {
  y_mean <- model_object$mean
  gamma_vec <- model_object$gamma
  tree_list <- model_object$tree
  n_iterations <- model_object$iter
  
  y_predicted <- rep(y_mean, (nrow(x_values)))
  h_i <- function(tree_model) {
    prediction <- predict(tree_model, x_values)
    return(prediction)
  }
  
  for (i in 1:n_iterations) {
    print(tree_list[[i]])
    y_predicted <- y_predicted + gamma_vec[i] * h_i(tree_list[[i]])
  }
  return(y_predicted)
}
