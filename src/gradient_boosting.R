library(rpart)

gradient_boosting <- function(x_values, y_values, n_iterations) {
  y_mean <- mean(y_values)
  y_predicted <- rep(y_mean, length(y_values))
  
  gamma_vec <- rep(NA, n_iterations)
  tree_list <- list()
  
  h_i <- function(tree_fit) {
    prediction <- predict(tree_fit, data = as.data.frame(x_values))
    return(prediction)
  }
  
  for (i in 1:n_iterations) {
    # Get pseudo residuals -----------------------------------------------------
    residuals <- y_values - y_predicted
    
    # Train tree predictor -----------------------------------------------------
    cart_training_set <- data.frame(output = residuals,
                                    x_values)
    tree_fit <- rpart(output ~ ., data = cart_training_set)
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
    gamma_vec[i] <- best_gamma
    
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
  class(model) <- 'gradient_boost'
  return(model)
}

predict.gradient_boost <- function(model_object, x_values) {
  y_mean <- model_object$mean
  gamma_vec <- model_object$gamma
  tree_list <- model_object$tree
  n_iterations <- model_object$iter
  
  y_predicted <- rep(y_mean, (nrow(x_values)))
  h_i <- function(tree_fit) {
    prediction <- predict(tree_fit, newdata = as.data.frame(x_values))
    return(prediction)
  }
  
  for (i in 1:n_iterations) {
    y_predicted <- y_predicted + gamma_vec[i] * h_i(tree_list[[i]])
  }
  return(y_predicted)
}
