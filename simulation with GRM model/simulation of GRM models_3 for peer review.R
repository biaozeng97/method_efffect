###########################
###  One factor model   ###
###########################

library(mirt)

num_items <- 20        # Number of items
num_respondents <- 250 # Number of respondents
num_categories <- 4
num_intercepts <- num_categories - 1
num_replications <- 10  # Number of replications

# Initialize matrices to store statistical data
bias_a_matrix <- matrix(NA, nrow = num_replications, ncol = num_items)
rmse_a_matrix <- matrix(NA, nrow = num_replications, ncol = num_items)
cor_a_matrix <- array(NA, dim = c(num_replications, 1, 1))

bias_d_matrix <- array(NA, dim = c(num_replications, num_items, num_intercepts))
rmse_d_matrix <- array(NA, dim = c(num_replications, num_items, num_intercepts))
cor_d_matrix <- array(NA, dim = c(num_replications, num_intercepts, num_intercepts))

bias_theta_matrix <- array(NA, dim = c(num_replications, num_respondents, 1))
rmse_theta_matrix <- array(NA, dim = c(num_replications, num_respondents, 1))
cor_theta_matrix <- array(NA, dim = c(num_replications, 1, 1))

for(rep in 1:num_replications) {
  set.seed(rep+200)  # Ensure reproducibility
  
  # Generate parameters a and d
  a <- matrix(runif(num_items, min = 0.5, max = 2), nrow = num_items)
  d <- matrix(nrow = num_items, ncol = num_intercepts)
  for(i in 1:num_items) {
    d[i, 1] <- runif(1, min = 1, max = 3)
    d[i, 2] <- runif(1, min = -1, max = 1)
    d[i, 3] <- runif(1, min = -3, max = -1)
  }
  
  # Define the one-factor model
  model <- 'F = 1-20'
  
  # Generate Theta
  Theta <- matrix(rnorm(num_respondents), nrow = num_respondents)
  
  # Simulate data
  simulated_data <- simdata(a = a, d = d, Theta = Theta, itemtype = 'graded', N = num_respondents)
  mirt_model <- mirt(simulated_data, model, itemtype = 'graded')
  
  # Check convergence
  if (!extract.mirt(mirt_model, 'converged')) {
    cat("Replication", rep, "did not converge.\n")
    next  # Skip to the next replication
  }
  
  # Extract estimated item parameters
  estimated_params <- coef(mirt_model, simplify = TRUE)
  estimated_params <- as.data.frame(estimated_params)
  estimated_a <- estimated_params[, 1]
  estimated_d <- estimated_params[, 2:4]
  estimated_theta <- fscores(mirt_model)
  
  # Calculate Bias and RMSE
  true_a <- a
  true_d <- d
  
  # Store Bias and RMSE for each item
  bias_a_matrix[rep, ] <- estimated_a - true_a
  rmse_a_matrix[rep, ] <- sqrt((estimated_a - true_a)^2)
  cor_a_matrix[rep, , ] <- cor(estimated_a, true_a, use = "complete.obs")
  
  for (i in 1:num_intercepts) {
    bias_d_matrix[rep, , i] <- estimated_d[, i] - d[, i]
    rmse_d_matrix[rep, , i] <- sqrt((estimated_d[, i] - d[, i])^2)
    cor_d_matrix[rep, , i] <- cor(estimated_d[, i], d[, i], use = "complete.obs")
  }
  
  # Store Bias and RMSE for each Theta
  bias_theta_matrix[rep, , 1] <- estimated_theta[, 1] - Theta[, 1]
  rmse_theta_matrix[rep, , 1] <- sqrt((estimated_theta[, 1] - Theta[, 1])^2)
  cor_theta_matrix[rep, , ] <- cor(estimated_theta[, 1], Theta[, 1])
}

# Calculate the mean Bias and RMSE for each item
mean_bias_a_per_item <- apply(bias_a_matrix, 2, mean, na.rm = TRUE)
mean_rmse_a_per_item <- apply(rmse_a_matrix, 2, mean, na.rm = TRUE)
mean_cor_a_per_item <- apply(cor_a_matrix, 2, mean, na.rm = TRUE)
mean_bias_d_per_item <- apply(bias_d_matrix, c(2, 3), mean, na.rm = TRUE)
mean_rmse_d_per_item <- apply(rmse_d_matrix, c(2, 3), mean, na.rm = TRUE)
mean_cor_d_per_item <- apply(cor_d_matrix, c(2, 3), mean, na.rm = TRUE)

# Calculate the mean Bias, RMSE, and correlation for Theta parameters
mean_bias_theta <- apply(bias_theta_matrix, c(2, 3), mean, na.rm = TRUE)
mean_rmse_theta <- apply(rmse_theta_matrix, c(2, 3), mean, na.rm = TRUE)
mean_cor_theta <- apply(cor_theta_matrix, c(2, 3), mean, na.rm = TRUE)

# Calculate overall mean for Bias, RMSE, and correlation for a parameters
overall_mean_bias_a <- mean(mean_bias_a_per_item, na.rm = TRUE)
overall_mean_rmse_a <- mean(mean_rmse_a_per_item, na.rm = TRUE)
overall_mean_cor_a <- mean(mean_cor_a_per_item, na.rm = TRUE)

# Calculate overall mean for Bias, RMSE, and correlation for d parameters
overall_mean_bias_d <- apply(mean_bias_d_per_item, 2, mean, na.rm = TRUE)
overall_mean_rmse_d <- apply(mean_rmse_d_per_item, 2, mean, na.rm = TRUE)
overall_mean_cor_d <- apply(mean_cor_d_per_item, 2, mean, na.rm = TRUE)

# Calculate overall mean for Bias, RMSE, and correlation for theta parameters
overall_mean_bias_theta <- mean(mean_bias_theta, na.rm = TRUE)
overall_mean_rmse_theta <- mean(mean_rmse_theta, na.rm = TRUE)
overall_mean_cor_theta <- mean(mean_cor_theta, na.rm = TRUE)

# Combine bias
combined_bias <- cbind(mean_bias_a_per_item, mean_bias_d_per_item, mean_rmse_a_per_item, mean_rmse_d_per_item)
colnames(combined_bias) <- c("a", paste0("d", 1:3), "a", paste0("d", 1:3))
combined_average_mean <- c(overall_mean_bias_a, overall_mean_bias_d, overall_mean_rmse_a, overall_mean_rmse_d)
combined_bias_rmse <- rbind(combined_bias, combined_average_mean)
combined_bias_rmse <- round(combined_bias_rmse, 3)

combined_average_mean_cor <- c(overall_mean_cor_a, overall_mean_cor_d)
combined_average_mean_cor <- round(combined_average_mean_cor, 3)

combined_theta_bias_rmse_cor <- c(overall_mean_bias_theta, overall_mean_rmse_theta, overall_mean_cor_theta)
combined_theta_bias_rmse_cor <- round(combined_theta_bias_rmse_cor, 3)

# Output the results
print("Combined Bias and RMSE for a and d parameters:")
print(combined_bias_rmse)

print("Average correlation for a and d parameters:")
print(combined_average_mean_cor)

print("Overall Bias, RMSE, and Correlation for Theta:")
print(combined_theta_bias_rmse_cor)





###########################
###   Bi-factor model   ###
###########################

library(mirt)

num_items <- 20        # Number of items
num_respondents <- 250 # Number of respondents
num_categories <- 4
num_intercepts <- num_categories - 1
num_replications <- 10 # Number of replications

# Initialize storage for statistics
bias_a_matrix <- array(NA, dim = c(num_replications, num_items, 3))
rmse_a_matrix <- array(NA, dim = c(num_replications, num_items, 3))
cor_a_matrix <- array(NA, dim = c(num_replications, 3, 3))

bias_d_matrix <- array(NA, dim = c(num_replications, num_items, num_intercepts))
rmse_d_matrix <- array(NA, dim = c(num_replications, num_items, num_intercepts))
cor_d_matrix <- array(NA, dim = c(num_replications, num_intercepts, num_intercepts))

bias_theta_matrix <- array(NA, dim = c(num_replications, num_respondents, 3))
rmse_theta_matrix <- array(NA, dim = c(num_replications, num_respondents, 3))
cor_theta_matrix <- array(NA, dim = c(num_replications, 3, 3))

for (rep in 1:num_replications) {
  set.seed(rep + 200)  # For reproducibility
  
  # Generate parameters a and d
  a <- matrix(NA, nrow = num_items, ncol = 3) # Initialize with NA
  d <- matrix(NA, nrow = num_items, ncol = num_intercepts * 3) # Initialize with NA
  
  # Generate parameters a and d
  # a and d for General factor (applies to all items)
  a[, 1] <- runif(num_items, min = 0.5, max = 2)
  
  # a and d for Specific factor 1 (first 8 items)
  a[1:8, 2] <- runif(8, min = 0.5, max = 2)
  
  # a and d for Specific factor 2 (last 12 items)
  a[9:20, 3] <- runif(12, min = 0.5, max = 2)
  
  d <- matrix(nrow = num_items, ncol = num_intercepts)
  for (i in 1:num_items) {
    d[i, 1] <- runif(1, min = 1, max = 3)
    d[i, 2] <- runif(1, min = -1, max = 1)
    d[i, 3] <- runif(1, min = -3, max = -1)
  }
  
  # Define the bi-factor model
  model <- 'G = 1-20
            S1 = 1-8
            S2 = 9-20'
  
  # Generate Theta
  Theta <- matrix(rnorm(num_respondents * 3), nrow = num_respondents, ncol = 3)
  
  # Simulate data
  simulated_data <- simdata(a = a, d = d, Theta = Theta, itemtype = 'graded', N = num_respondents)
  mirt_model <- bfactor(simulated_data, c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), itemtype = "graded")
  
  # extract.mirt(x, 'what') 
  # converged : a logical value indicating whether the model terminated within the convergence criteria
  # Check convergence
  if (!extract.mirt(mirt_model, 'converged')) {
    cat("Replication", rep, "did not converge.\n")
    next  # Skip to the next replication
  }
  
  
  # Extract estimated item parameters
  estimated_params <- coef(mirt_model, simplify = TRUE)
  estimated_params <- as.data.frame(estimated_params$items)
  estimated_a <- estimated_params[, 1:3]
  estimated_d <- estimated_params[, 4:6]
  estimated_theta <- fscores(mirt_model)
  
  # Calculate Bias and RMSE for each item
  for (i in 1:3) {
    bias_a_matrix[rep, , i] <- estimated_a[, i] - a[, i]
    rmse_a_matrix[rep, , i] <- sqrt((estimated_a[, i] - a[, i])^2)
    cor_a_matrix[rep, , i] <- cor(estimated_a[, i], a[, i], use = "complete.obs")
  }
  
  for (i in 1:num_intercepts) {
    bias_d_matrix[rep, , i] <- estimated_d[, i] - d[, i]
    rmse_d_matrix[rep, , i] <- sqrt((estimated_d[, i] - d[, i])^2)
    cor_d_matrix[rep, , i] <- cor(estimated_d[, i], d[, i])
  }
  
  # Calculate Bias and RMSE for each theta
  for (i in 1:3) {
    bias_theta_matrix[rep, , i] <- estimated_theta[, i] - Theta[, i]
    rmse_theta_matrix[rep, , i] <- sqrt((estimated_theta[, i] - Theta[, i])^2)
    cor_theta_matrix[rep, i, ] <- cor(estimated_theta[, i], Theta[, i])
  }
}

# Calculate mean Bias and RMSE per item
mean_bias_a_per_item <- apply(bias_a_matrix, c(2, 3), mean, na.rm = TRUE)
mean_rmse_a_per_item <- apply(rmse_a_matrix, c(2, 3), mean, na.rm = TRUE)
mean_cor_a_per_item <- apply(cor_a_matrix, c(2, 3), mean, na.rm = TRUE)

mean_bias_d_per_item <- apply(bias_d_matrix, c(2, 3), mean, na.rm = TRUE)
mean_rmse_d_per_item <- apply(rmse_d_matrix, c(2, 3), mean, na.rm = TRUE)
mean_cor_d_per_item <- apply(cor_d_matrix, c(2, 3), mean, na.rm = TRUE)

mean_bias_theta <- apply(bias_theta_matrix, c(2, 3), mean, na.rm = TRUE)
mean_rmse_theta <- apply(rmse_theta_matrix, c(2, 3), mean, na.rm = TRUE)
mean_cor_theta <- apply(cor_theta_matrix, c(2, 3), mean, na.rm = TRUE)



# Calculate overall mean for Bias, RMSE, and correlation for a parameters
overall_mean_bias_a <- apply(mean_bias_a_per_item, 2, mean, na.rm = TRUE)
overall_mean_rmse_a <- apply(mean_rmse_a_per_item, 2, mean, na.rm = TRUE)
overall_mean_cor_a <- apply(mean_cor_a_per_item, 2, mean, na.rm = TRUE)

# Calculate overall mean for Bias, RMSE, and correlation for d parameters
overall_mean_bias_d <- apply(mean_bias_d_per_item, 2, mean, na.rm = TRUE)
overall_mean_rmse_d <- apply(mean_rmse_d_per_item, 2, mean, na.rm = TRUE)
overall_mean_cor_d <- apply(mean_cor_d_per_item, 2, mean, na.rm = TRUE)

# Calculate overall mean for Bias, RMSE, and correlation for theta parameters
overall_mean_bias_theta <- apply(mean_bias_theta, 2, mean, na.rm = TRUE)
overall_mean_rmse_theta <- apply(mean_rmse_theta, 2, mean, na.rm = TRUE)
overall_mean_cor_theta <- apply(mean_cor_theta, 2, mean, na.rm = TRUE)




# combine bias
combined_bias <- cbind(mean_bias_a_per_item, mean_bias_d_per_item, mean_rmse_a_per_item, mean_rmse_d_per_item)
colnames(combined_bias) <- c(paste0("a", 1:3), paste0("d", 1:3), paste0("a", 1:3), paste0("d", 1:3))
combined_average_mean <- c(overall_mean_bias_a, overall_mean_bias_d, overall_mean_rmse_a, overall_mean_rmse_d)
combined_bias_rmse <- rbind(combined_bias, combined_average_mean)
combined_bias_rmse <- round(combined_bias_rmse, 3)

combined_average_mean_cor <- c(overall_mean_cor_a, overall_mean_cor_d)
combined_average_mean_cor <- round(combined_average_mean_cor, 3)

combined_theta_bias_rmse_cor <- c(overall_mean_bias_theta, overall_mean_rmse_theta, overall_mean_cor_theta)
combined_theta_bias_rmse_cor <- round(combined_theta_bias_rmse_cor, 3)
