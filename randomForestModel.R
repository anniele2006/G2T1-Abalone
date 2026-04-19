# load libraries
library(tidyverse)
library(randomForest)

# Read in the abalone dataset and assign column names
abalone <- read_csv(
  "abalone/abalone.data",
  col_names = c(
    "Sex", "Length", "Diameter", "Height",
    "Whole_Weight", "Shucked_Weight", "Viscera_Weight",
    "Shell_Weight", "Rings"
  )
)

# Convert Sex to a factor so the model treats it as a categorical predictor
abalone$Sex <- as.factor(abalone$Sex)

# Set a seed so the cross-validation results are reproducible
set.seed(123)

# Define the number of folds for cross-validation
k <- 10

# Create folds once so every model uses the same splits
folds <- sample(rep(1:k, length.out = nrow(abalone)))

# Reusable function for k-fold CV random forest
run_rf_cv <- function(data, formula, folds, ntree = 500) {
  k <- length(unique(folds))
  
  rmse_vals <- numeric(k)
  mae_vals  <- numeric(k)
  r2_vals   <- numeric(k)
  
  for (i in 1:k) {
    train_data <- data[folds != i, ]
    test_data  <- data[folds == i, ]
    
    rf_fit <- randomForest(
      formula,
      data = train_data,
      ntree = ntree,
      importance = TRUE
    )
    
    preds <- predict(rf_fit, newdata = test_data)
    actual <- test_data$Rings
    
    rmse_vals[i] <- sqrt(mean((preds - actual)^2))
    mae_vals[i]  <- mean(abs(preds - actual))
    
    # R-squared for this fold
    sse <- sum((actual - preds)^2)
    sst <- sum((actual - mean(actual))^2)
    r2_vals[i] <- 1 - sse / sst
  }
  
  list(
    rmse = rmse_vals,
    mae = mae_vals,
    r2 = r2_vals,
    avg_rmse = mean(rmse_vals),
    avg_mae = mean(mae_vals),
    avg_r2 = mean(r2_vals)
  )
}

# 1) Full model: uses all available predictors

cv_full <- run_rf_cv(
  data = abalone,
  formula = Rings ~ .,
  folds = folds
)


# 2) Non-destructive model uses only predictors that do not require opening the abalone

cv_non_destructive <- run_rf_cv(
  data = abalone,
  formula = Rings ~ Sex + Length + Diameter + Height + Whole_Weight,
  folds = folds
)

# Compare cross-validation results
comparison <- tibble(
  Model = c("Full model", "Non-destructive model"),
  RMSE = c(cv_full$avg_rmse, cv_non_destructive$avg_rmse),
  MAE  = c(cv_full$avg_mae, cv_non_destructive$avg_mae),
  R2   = c(cv_full$avg_r2, cv_non_destructive$avg_r2)
)

print(comparison)


# Fit final models on the full dataset

# Final full model
final_rf_full <- randomForest(
  Rings ~ .,
  data = abalone,
  ntree = 500,
  importance = TRUE
)

# Final non-destructive model
final_rf_non_destructive <- randomForest(
  Rings ~ Sex + Length + Diameter + Height + Whole_Weight,
  data = abalone,
  ntree = 500,
  importance = TRUE
)

# Show variable importance for each final model
importance(final_rf_full)
varImpPlot(final_rf_full)

importance(final_rf_non_destructive)
varImpPlot(final_rf_non_destructive)
