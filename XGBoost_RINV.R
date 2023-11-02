################# XGBOOST WITH ROLLING WINDOW ####################
#Define file path
library(readr)
act_data <- read_delim("act_data_INVEST.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
act_data <- as.data.frame(act_data)
data2 <- act_data[1:52,] #omit covid period (data to forecast)
setwd("/Users/sofiagioacchini/Documents/LMEC/Machine learning")


######### 1-STEP-AHEAD FORECAST ############
window_size <- 52
forecast_horizon <- 1

# Number of rolling windows
n_windows <- 26-forecast_horizon+1

# Initialize a vector to store mean squared errors
mse_values <- numeric(n_windows)

for (i in 1:n_windows) {
  # Define the rolling window range
  window_start <- i
  window_end <- i + window_size - 1
  
  # Split the data into training and testing based on the rolling window
  train_data <- as.matrix(act_data[window_start:(window_start + window_size - 1), ])
  train_labels <- train_data[, "PRFIx"]
  train_feat <- train_data[, -which(colnames(train_data) == "PRFIx")]
  
  test_data <- as.matrix(act_data[-(window_start:(window_start + window_size - 1)), ])
  test_labels <- test_data[, "PRFIx"]
  test_feat <- test_data[, -which(colnames(test_data) == "PRFIx")]
  
  # Train an XGBoost model
  XG_model <- xgboost(data = data.matrix(train_feat), 
                      label = train_labels, 
                      max.depth = 2,
                      nrounds = 10)
  
  # Make predictions on the test set
  predictions <- predict(XG_model, newdata = data.matrix(test_feat))
  
  # Calculate and store the mean squared error
  mse <- mean((predictions - test_labels)^2)
  mse_values[i] <- mse
}

# Print the mean squared errors for each rolling window
cat("Mean Squared Errors (MSE) for each window:\n")
print(mse_values)

# Overall MSE
overall_mse <- mean(mse_values)
cat("Overall Mean Squared Error (MSE):", overall_mse, "\n")  #1.258077

#Plot them features: what's contributing most to our model?
#install.packages("DiagrammeR")
library(DiagrammeR)

xgb.plot.multi.trees(feature_names = names(act_data), 
                     model = XG_model,
                     fill=TRUE )


# Extract feature names from the dataset
feature_names <- names(act_data)
feature_names <- feature_names[-1]
# Check the feature names used by the XGBoost model
model_feature_names <- XG_model$feature_names
# Ensure they match in length
if (length(feature_names) != length(model_feature_names)) {
  stop("Mismatch in the number of feature names.")
}

#Get information on how important each feature is
importance_matrix <- xgb.importance(feature_names, model = XG_model)

# and plot it
xgb.plot.importance(importance_matrix)

# Assuming you have 'predictions' and 'test_labels' as vectors
# 'predictions' contains the XGBoost predictions, and 'test_labels' contains the actual data

# Create a sequence of time steps for the x-axis (1, 2, 3, ...)
time_steps <- 1:length(test_labels)

# Plot the actual data and predictions in a line plot
matplot(time_steps, cbind(test_labels, predictions), type = "l", lty = 1,
        col = c("black", "purple"), lwd = 2, xlab = "Quarters", ylab = "Res. investment",
        main = "Actual vs. 1-step ahead XGBoost prediction")

# Add a legend to the plot
legend("topright", legend = c("Actual", "Predicted"), col = c("black", "purple"), lty = 1, lwd = 2)


#####4-STEP-AHEAD FORECAST#####
######### 4-STEP-AHEAD FORECAST ############
window_size <- 52
forecast_horizon <- 4

# Number of rolling windows
n_windows <- 26-forecast_horizon+1

# Initialize a vector to store mean squared errors
mse_values <- numeric(n_windows)

for (i in 1:n_windows) {
  # Define the rolling window range
  window_start <- i
  window_end <- i + window_size - 1
  
  # Split the data into training and testing based on the rolling window
  train_data <- as.matrix(act_data[window_start:(window_start + window_size - 1), ])
  train_labels <- train_data[, "PRFIx"]
  train_feat <- train_data[, -which(colnames(train_data) == "PRFIx")]
  
  test_data <- as.matrix(act_data[-(window_start:(window_start + window_size - 1)), ])
  test_labels <- test_data[, "PRFIx"]
  test_feat <- test_data[, -which(colnames(test_data) == "PRFIx")]
  
  # Train an XGBoost model
  XG_model <- xgboost(data = data.matrix(train_feat), 
                      label = train_labels, 
                      max.depth = 1,
                      nrounds = 10)
  
  # Make predictions on the test set
  predictions <- predict(XG_model, newdata = data.matrix(test_feat))
  
  # Calculate and store the mean squared error
  mse <- mean((predictions - test_labels)^2)
  mse_values[i] <- mse
}

# Print the mean squared errors for each rolling window
cat("Mean Squared Errors (MSE) for each window:\n")
print(mse_values)

# Overall MSE
overall_mse <- mean(mse_values)
cat("Overall Mean Squared Error (MSE):", overall_mse, "\n")   #0.06111778

#Plot them features: what's contributing most to our model?
#install.packages("DiagrammeR")
library(DiagrammeR)

xgb.plot.multi.trees(feature_names = names(act_data), 
                     model = XG_model,
                     fill=TRUE )


# Extract feature names from the dataset
feature_names <- names(act_data)
feature_names <- feature_names[-1]
# Check the feature names used by the XGBoost model
model_feature_names <- XG_model$feature_names
# Ensure they match in length
if (length(feature_names) != length(model_feature_names)) {
  stop("Mismatch in the number of feature names.")
}

#Get information on how important each feature is
importance_matrix <- xgb.importance(feature_names, model = XG_model)

# and plot it
xgb.plot.importance(importance_matrix)

#PLOT
# Create a sequence of time steps for the x-axis (1, 2, 3, ...)
time_steps <- 1:length(test_labels)

# Plot the actual data and predictions in a line plot
matplot(time_steps, cbind(test_labels, predictions), type = "l", lty = 1,
        col = c("black", "purple"), lwd = 2, xlab = "Quarters", ylab = "Res. investment",
        main = "Actual vs. 4-step ahead XGBoost prediction")

# Add a legend to the plot
legend("topright", legend = c("Actual", "Predicted"), col = c("black", "purple"), lty = 1, lwd = 2)

