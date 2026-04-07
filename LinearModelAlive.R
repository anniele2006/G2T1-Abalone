# Load data (no header in .data file)
alivebalone <- read.csv("abalone.data", header = FALSE)

colnames(alivebalone) <- c("Sex", "Length", "Diameter", "Height",
                       "WholeWeight", "ShuckedWeight", "VisceraWeight",
                       "ShellWeight", "Rings")
alivebalone <- alivebalone[, !names(alivebalone) %in% 
                                 c("ShuckedWeight", "ShellWeight", "VisceraWeight")]


# Encode Sex as a factor
alivebalone$Sex <- as.factor(alivebalone$Sex)

# Quick look and removing zeroes from height as seen from summary
summary(alivebalone)
alivebalone <- alivebalone [alivebalone$Height > 0,]

lm_alive_full <- lm(Rings ~ Sex + Length + Diameter + Height +
                WholeWeight,
              data = alivebalone)

par(mfrow = c(2, 2))
plot(lm_alive_full)

#Removing outliers
# Check the observation first before removing
alivebalone[2051, ]
alivebalone[1417, ]

# Remove it
alivebalone <- alivebalone[-2051, ]
alivebalone <- alivebalone[-1417, ]

#Replot to check assumptions
lm_alive_full <- lm(Rings ~ Sex + Length + Diameter + Height +
                      WholeWeight,
                    data = alivebalone)
par(mfrow = c(2, 2))
plot(lm_alive_full)


#Trying to find optimal model for response variable
library(MASS)
boxcox(lm_alive_full)

bc_alive <- boxcox(lm_alive_full)
best_alive_lambda <- bc$x[which.max(bc$y)]
best_alive_lambda

#As best lambda is -0.06. we use log(rings). Test for assumptions again
lm_alive_log <- lm (log(Rings) ~ Sex + Length + Diameter + Height +
                WholeWeight,
              data = alivebalone)

par (mfrow = c(2,2))
plot (lm_alive_log)

#Check which to change into predictors
library(car)
crPlots(lm_alive_log)

#Transform necessary predictors
lm_alive_transformed <- lm(log(Rings) ~ Sex + Length + Diameter + Height +
                       WholeWeight
                       , data = alivebalone)

par(mfrow = c(2, 2))
plot(lm_alive_transformed)


#Stepwise, Backward
step_alive_backward <- stepAIC(lm_alive_transformed, 
                         direction = "backward", 
                         trace = TRUE)  # trace=TRUE shows each step

summary(step_alive_backward)

#Stepwise, Foreward
lm_alive_null <- lm(log(Rings) ~ 1, data = alivebalone)
step_alive_forward <- stepAIC(lm_alive_null,
                        direction = "forward",
                        scope = list(lower = lm_alive_null, 
                                     upper = lm_alive_transformed),
                        trace = TRUE)

summary(step_alive_forward)

#Exhaustive Search
# Manually create dummy variables for Sex
alivebalone$SexI <- as.integer(alivebalone$Sex == "I")
alivebalone$SexM <- as.integer(alivebalone$Sex == "M")

exhaustive_alive <- regsubsets(log(Rings) ~ Sex + Length + Diameter + Height +
                           WholeWeight, 
                         data = alivebalone,
                         nbest = 1,        # best model of each size
                         nvmax = 10,       # max predictors to consider
                         method = "exhaustive")

# Summary with different criteria
exhaustive_alive_summary <- summary(exhaustive_alive)

# Find best model by each criterion
best_alive_bic   <- which.min(exhaustive_alive_summary$bic)

cat("Best size by BIC:     ", best_alive_bic, "\n")

# Fit the best model (using BIC as primary criterion)
best_alive_vars <- names(coef(exhaustive, best_alive_bic))[-1]
best_alive_formula <- as.formula(paste("Rings ~", paste(best_alive_vars, collapse = " + ")))
best_alive_exhaustive_model <- lm(best_alive_formula, data = alivebalone)
summary_alive_best <- summary(best_alive_exhaustive_model)
print(summary_alive_best)
plot(exhaustive_alive, scale = "bic")

# For the all-in-one diagnostic plot
par(mfrow = c(2, 2),
    mar = c(5, 5, 4, 2),      # wider margins
    cex.main = 1.5,            # bigger title text
    cex.lab = 1.3,             # bigger axis labels
    cex.axis = 1.2)            # bigger axis numbers
plot(best_alive_exhaustive_model)


# Compare the two candidate models directly
lm_alive_stepwise <- lm(log(Rings) ~ SexI + Length + Diameter + Height +
                    WholeWeight
                    , data = alivebalone)

lm_alive_exhaustive <- lm(log(Rings) ~ SexI + Diameter + Height +
                      WholeWeight
                      , data = alivebalone)

# Direct comparison to see whether to drop log(length)
AIC(lm_alive_stepwise, lm_alive_exhaustive)
BIC(lm_alive_stepwise, lm_alive_exhaustive)

#Final linear model
lm_alive_final <- lm(log(Rings) ~ SexI + Diameter + Height +
                 WholeWeight
                 , data = alivebalone)

summary(lm_alive_final)


#Set seed for 10-fold cross validation
set.seed(123)

install.packages("caret")
library(caret)

# Since we're predicting log(Rings), add it as a column
alivebalone$logRings <- log(alivebalone$Rings)

# Define 10-fold CV control
train_alive_control <- trainControl(method = "cv", 
                              number = 10)

cv_alive_final <- train(logRings ~ SexI + Diameter + Height +
                    WholeWeight,
                  data = alivebalone,
                  method = "lm",
                  trControl = train_alive_control)

# View results
cv_final
