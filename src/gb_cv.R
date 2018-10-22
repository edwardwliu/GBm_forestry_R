library(rpart)

gradient_boosting_cv <- function(x_values, y_values, n_iterations) {
  y_mean <- mean(y_values)
  y_predicted <- rep(y_mean, length(y_values))
  
  gamma_vec <- rep(NA, n_iterations)
  tree_list <- list()
  
  h_i <- function(tree_model, test_values) {
    prediction <-
      predict(tree_model, newdata = as.data.frame(test_values))
    return(prediction)
  }
  
  all_data = cbind(x_values, y_values)
  
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
    
    # Use cross-validation to determine gamma
    # 1. Shuffle data and create folds
    cv_data = cbind(all_data, data.frame(y_predicted))
    cv_data = cbind(cv_data, data.frame(residuals))
    cv_data <- cv_data[sample(nrow(x_values)),]
    folds <- cut(seq(1, nrow(x_values)), breaks = 5, labels = FALSE)
    num_col = ncol(cv_data)
    
    for (k in 1:5) {
      # 2. Get training and test sets
      test_indices <- which(folds == k, arr.ind = TRUE)
      test_data_x <-
        cv_data[test_indices,][,-num_col][,-(num_col - 1)][,-(num_col - 2)]
      test_data_y <- cv_data[test_indices,][, (num_col - 2)]
      test_data_predicted <-
        cv_data[test_indices,][, (num_col - 1)]
      train_data_x <-
        cv_data[-test_indices,][,-num_col][,-(num_col - 1)][,-(num_col - 2)]
      train_data_residuals <- cv_data[-test_indices,][, num_col]
      
      # 3. Train a new tree for each fold
      fold_train_set <- data.frame(output = train_data_residuals,
                                   train_data_x)
      fold_fit <- rpart(output ~ ., data = fold_train_set)
      
      # 4. Test gamma values for each fold's test data
      for (gamma in gamma_grid) {
        fitted_values_gamma <-
          test_data_predicted + gamma * h_i(fold_fit, test_data_x)
        current_val <- mean((test_data_y - fitted_values_gamma) ^ 2)
        if (best_min > current_val) {
          best_min <- current_val
          best_gamma <- gamma
        }
      }
    }
    
    gamma_vec[i] <- best_gamma
    
    y_predicted <- rep(y_mean, length(y_values))
    for (j in 1:i) {
      y_predicted <-
        y_predicted + gamma_vec[j] * h_i(tree_list[[j]], x_values)
    }
  }
  
  # Return model ---------------------------------------------------------------
  model <- list(
    mean = y_mean,
    gamma = gamma_vec,
    tree = tree_list,
    iter = n_iterations
  )
  class(model) <- 'gradient_boost_cv'
  return(model)
}

predict.gradient_boost_cv <- function(model_object, x_values) {
  y_mean <- model_object$mean
  gamma_vec <- model_object$gamma
  tree_list <- model_object$tree
  n_iterations <- model_object$iter
  
  y_predicted <- rep(y_mean, (nrow(x_values)))
  h_i <- function(tree_model, test_values) {
    prediction <-
      predict(tree_model, newdata = as.data.frame(test_values))
    return(prediction)
  }
  
  for (i in 1:n_iterations) {
    y_predicted <-
      y_predicted + gamma_vec[i] * h_i(tree_list[[i]], x_values)
  }
  return(y_predicted)
}
