# Load necessary libraries
library(tidyverse)
library(caret)
library(DescTools)
library(rpart)
library(dplyr)
library(rpart.plot)
library(pROC)

getwd()
# Load the dataset
framingham <- read.csv("framingham.csv")
data(framingham)  # assuming the dataset is loaded as 'framingham'
glimpse(framingham)
attach(framingham)

# Check for missing values
sum(is.na(framingham))
sum(is.na(education))  # 105
sum(is.na(cigsPerDay))  # 29
sum(is.na(BPMeds))  # 53
sum(is.na(totChol))  # 50
sum(is.na(BMI))  # 19
sum(is.na(heartRate))  # 1
sum(is.na(glucose))  # 388

# Replace missing values
heart <- replace_na(framingham, list(education = as.integer(Mode(education, na.rm = TRUE)),
                                     BPMeds = as.integer(Mode(BPMeds, na.rm = TRUE)), 
                                     cigsPerDay = as.integer(mean(cigsPerDay, na.rm = TRUE)),
                                     totChol = as.integer(mean(totChol, na.rm = TRUE)),
                                     BMI = mean(BMI, na.rm = TRUE),
                                     heartRate = as.integer(mean(heartRate, na.rm = TRUE)),
                                     glucose = as.integer(mean(glucose, na.rm = TRUE))))
sum(is.na(heart))

#Features
# Breaks to create bins
breaks <- c(0, 200, 239, max(heart$totChol))  # the bin boundaries for tot_chol
#Variable interactions
heart <- heart %>%
  mutate(currentSmoker_cigsPerDay=currentSmoker*cigsPerDay,
         currentSmoker_prevalentHyp = currentSmoker * prevalentHyp,
         age_heartRate = age * heartRate,
         age_BMI = age * BMI,
         age_cigsPerDay = age * cigsPerDay,
         totChol_age = totChol * age,
         glucose_diabetes=diabetes*glucose,
         prevalentStroke_prevalentHyp = prevalentStroke * prevalentHyp,
         totChol_categories= cut(totChol, breaks = breaks, labels=c("Healthy","At-Risk","Dangerous"), include.lowest = TRUE),)


#Cross Validation -----------------

library(tree)
library(rpart)
library(MASS)

# Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(heart$TenYearCHD, p = 0.7, list = FALSE)
train_data <- heart[trainIndex, ]
test_data <- heart[-trainIndex, ]

# Fit the decision tree model
tree_model <- rpart(TenYearCHD ~ ., data = train_data, method = "class")

# Print the tree structure
print(tree_model)

# Plot the decision tree
rpart.plot(tree_model, type = 3, extra = 104, fallen.leaves = TRUE, main = "Decision Tree for heart Disease Prediction")

# Predictions
tree_predictions <- predict(tree_model, test_data, type = "class")

# Confusion matrix and accuracy
confusion_matrix <- table(tree_predictions, test_data$TenYearCHD)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")

# Predict probabilities for log loss calculation and ROC curve
tree_probabilities <- predict(tree_model, test_data, type = "prob")[,2]

# Function to calculate log loss
log_loss <- function(actual, predicted, epsilon = 1e-15) {
  predicted <- pmax(pmin(predicted, 1 - epsilon), epsilon)
  loss <- -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
  return(loss)
}

# Calculate log loss
log_loss_value <- log_loss(test_data$TenYearCHD, tree_probabilities)
cat("Log Loss:", log_loss_value, "\n")

# ROC curve and AUC
roc_curve <- roc(test_data$TenYearCHD, tree_probabilities)
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

# Plot ROC curve
plot(roc_curve, main = "ROC Curve for Decision Tree", col = "blue", lwd = 2)
abline(a = 0, b = 1, col = "red", lty = 2)


