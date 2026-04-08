# Load data (no header in .data file)
abalone <- read.csv("abalone.data", header = FALSE)

colnames(abalone) <- c("Sex", "Length", "Diameter", "Height",
                       "WholeWeight", "ShuckedWeight", "VisceraWeight",
                       "ShellWeight", "Rings")

# Encode Sex as a factor
abalone$Sex <- as.factor(abalone$Sex)

# Quick look and removing zeroes from height as seen from summary
summary(abalone)
abalone <- abalone [abalone$Height > 0,]

lm_full <- lm(Rings ~ Sex + Length + Diameter + Height +
                WholeWeight + ShuckedWeight + VisceraWeight + ShellWeight,
              data = abalone)

par(mfrow = c(2, 2))S
plot(lm_full)

#Trying to find optimal model for response variable
library(MASS)
boxcox(lm_full)

bc <- boxcox(lm_full)
best_lambda <- bc$x[which.max(bc$y)]
best_lambda

#As best lambda is -0.06. we use log(rings). Test for assumptions again
lm_log <- lm (log(Rings) ~ Sex + Length + Diameter + Height +
              WholeWeight + ShuckedWeight + VisceraWeight + ShellWeight,
              data = abalone)

par (mfrow = c(2,2))
plot (lm_log)

#Transform necessary predictors
lm_transformed <- lm(log(Rings) ~ Sex + log(Length) + log(Diameter) + Height +
                       WholeWeight + log(ShuckedWeight) + log(VisceraWeight) +
                       ShellWeight, data = abalone)

# View the suspicious observations first
abalone[c(237, 2051), ]

# Removing the suspicious observations 
abalone <- abalone[!rownames(abalone) %in% c("237", "2052"), ]

par(mfrow = c(2, 2))
plot(lm_transformed)

#Stepwise, Backward
step_backward <- stepAIC(lm_transformed, 
                         direction = "backward", 
                         trace = TRUE)  # trace=TRUE shows each step

summary(step_backward)

#Stepwise, Foreward
lm_null <- lm(log(Rings) ~ 1, data = abalone)
step_forward <- stepAIC(lm_null,
                        direction = "forward",
                        scope = list(lower = lm_null, 
                                     upper = lm_transformed),
                        trace = TRUE)

summary(step_forward)

#Exhaustive Search
# Manually create dummy variables for Sex
abalone$SexI <- as.integer(abalone$Sex == "I")
abalone$SexM <- as.integer(abalone$Sex == "M")

exhaustive <- regsubsets(log(Rings) ~ Sex + log(Length) + log(Diameter) + Height +
                           WholeWeight + log(ShuckedWeight) + log(VisceraWeight) +
                           ShellWeight, 
                         data = abalone,
                         nbest = 1,        # best model of each size
                         nvmax = 10,       # max predictors to consider
                         method = "exhaustive")

# Summary with different criteria
exhaustive_summary <- summary(exhaustive)

# Find best model by each criterion
best_bic   <- which.min(exhaustive_summary$bic)

cat("Best size by BIC:     ", best_bic, "\n")

# Fit the best model (using BIC as primary criterion)
best_vars <- names(coef(exhaustive, best_bic))[-1]
best_formula <- as.formula(paste("Rings ~", paste(best_vars, collapse = " + ")))
best_exhaustive_model <- lm(best_formula, data = abalone)
summary_best <- summary(best_exhaustive_model)
print(summary_best)
plot(exhaustive, scale = "bic")

# For the all-in-one diagnostic plot
par(mfrow = c(2, 2),
    mar = c(5, 5, 4, 2),      # wider margins
    cex.main = 1.5,            # bigger title text
    cex.lab = 1.3,             # bigger axis labels
    cex.axis = 1.2)            # bigger axis numbers
plot(best_exhaustive_model)


# Compare the two candidate models directly
lm_stepwise <- lm(log(Rings) ~ SexI + log(Length) + log(Diameter) + Height +
                    WholeWeight + log(ShuckedWeight) + log(VisceraWeight) +
                    ShellWeight, data = abalone)

lm_exhaustive <- lm(log(Rings) ~ SexI + log(Diameter) + Height +
                      WholeWeight + log(ShuckedWeight) + log(VisceraWeight) +
                      ShellWeight, data = abalone)

# Direct comparison to see whether to drop log(length)
AIC(lm_stepwise, lm_exhaustive)
BIC(lm_stepwise, lm_exhaustive)

#Final linear model
lm_final <- lm(log(Rings) ~ SexI + log(Diameter) + Height +
                 WholeWeight + log(ShuckedWeight) + log(VisceraWeight) +
                 ShellWeight, data = abalone)

summary(lm_final)


#Set seed for 10-fold cross validation
set.seed(123)

install.packages("caret")
library(caret)

# Since we're predicting log(Rings), add it as a column
abalone$logRings <- log(abalone$Rings)

# Define 10-fold CV control
train_control <- trainControl(method = "cv", 
                              number = 10)

cv_final <- train(logRings ~ SexI + log(Diameter) + Height +
                    WholeWeight + log(ShuckedWeight) + log(VisceraWeight) +
                    ShellWeight,
                  data = abalone,
                  method = "lm",
                  trControl = train_control)

# View results
cv_final

# Predictions
new_obs <- data.frame(
  SexI = 0,                    # not infant (e.g. adult)
  Diameter = 0.4,
  Height = 0.15,
  WholeWeight = 0.8,
  ShuckedWeight = 0.35,
  VisceraWeight = 0.18,
  ShellWeight = 0.25
)

predict(lm_final, new_obs, interval = "prediction", level = 0.90)

# Convert Back the Log..
exp(2.328883)  
exp(1.994742) 
exp(2.663024)