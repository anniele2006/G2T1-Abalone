# Load required libraries for data manipulation, modeling, and visualization
library(tidyverse)
library(randomForest)

# Read in the abalone dataset and assign column names
abalone <- read_csv(
  "abalone/abalone.data", 
  col_names = c("Sex", "Length", "Diameter", "Height", "Whole_Weight", "Shucked_Weight", "Viscera_Weight", "Shell_Weight", "Rings"
  )
)

# Convert Sex to a factor so the model treats it as a categorical predictor
abalone$Sex <- as.factor(abalone$Sex)

# Set a seed so the cross-validation results are reproducible
set.seed(123)

# Define the number of folds for cross-validation
k <- 10

# Randomly assign each observation to one of the 10 folds
folds <- sample(rep(1:k, length.out = nrow(abalone)))

# Create empty vectors to store model performance for each fold
rmse_vals <- c()
mae_vals <- c()
r2_vals <- c()

# Perform 10-fold cross-validation
for (i in 1:k) {
  
  # Use 9 folds for training and 1 fold for testing
  train_data <- abalone[folds != i, ]
  test_data  <- abalone[folds == i, ]
  
  # Fit a random forest regression model to predict Rings
  rf_fit <- randomForest(
    Rings ~ .,
    data = train_data,
    ntree = 500,
    importance = TRUE
  )
  
  # Generate predictions on the test fold
  preds <- predict(rf_fit, newdata = test_data)
  
  # Store evaluation metrics for this fold
  rmse_vals[i] <- sqrt(mean((preds - test_data$Rings)^2))
  mae_vals[i]  <- mean(abs(preds - test_data$Rings))
  r2_vals[i]   <- cor(preds, test_data$Rings)^2
}


# Compute average cross-validation performance across all 10 folds
# Average CV performance across 10 folds

# RMSE = 2.139932
# Root Mean Squared Error measures the typical prediction error in Rings (extra penalty to larger mistakes)
# odel is off by about 2.14 Rings on average, with bigger errors weighted more heavily.
mean(rmse_vals) 

# MAE = 1.516499
# Mean Absolute Error measures the average absolute difference between predicted and actual Rings.
# Model's predictions differ from the true value by about 1.52 Rings on average.
mean(mae_vals)

# R2 = 0.5587318
# R-squared measures how much of the variation in Rings is explained by the model.
# The random forest explains about 55.9% of the variability in ring count.
mean(r2_vals)

# Fit a final random forest model on full dataset
# This is used for interpreting variable importance after validation
final_rf <- randomForest(
  Rings ~ .,
  data = abalone,
  ntree = 500,
  importance = TRUE
)

# Display the importance of each predictor in the final model
importance(final_rf)

# %IncMSE: higher values mean the model depends more on that variable for prediction
# IncNodePurity: higher values mean the variable helps create better tree splits
# Weight-related variables appear to be the most important predictors of Rings
varImpPlot(final_rf)


