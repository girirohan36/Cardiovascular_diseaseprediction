# Load necessary libraries
library(tidyverse)
library(tree)
library(caret)
library(DescTools)

# Load the Framingham dataset
data(framingham)  # Replace this with actual data loading if necessary
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

# Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(heart$TenYearCHD, p = 0.7, list = FALSE)
train_data <- heart[trainIndex, ]
test_data <- heart[-trainIndex, ]

# Fit a large decision tree
temp_tree <- tree(TenYearCHD ~ ., data = train_data, mindev = 0.0001)
cat("First big tree size: \n")
print(length(unique(temp_tree$where)))

# Prune the tree to a desired size (e.g., 7 leaves)
pruned_tree <- prune.tree(temp_tree, best = 7)
cat("Pruned tree size: \n")
print(length(unique(pruned_tree$where)))

# Set a valid file path
output_path <- "C:/Users/girir/Documents/R_plots/"
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}
png(filename = paste0(output_path, "decision_tree_plot.png"), width = 800, height = 600)

# Plot the pruned tree and the fits
par(mfrow = c(1, 2))

# Plot the pruned tree
plot(pruned_tree, type = "uniform")
text(pruned_tree, col = "blue", label = c("yval"), cex = 0.8)
title("Pruned Decision Tree")

# Predict on training data to visualize the fit
train_predictions <- predict(pruned_tree, newdata = train_data, type = "vector")

# Plot data with fit
plot(train_data$age, train_data$TenYearCHD, cex = 0.5, pch = 16, xlab = "Age", ylab = "TenYearCHD")
oo <- order(train_data$age)
lines(train_data$age[oo], train_predictions[oo], col = "red", lwd = 3)  # step function fit

# Assuming the cutpoints from the tree (you may extract these from the tree structure if needed)
cvals <- c(45, 52, 60, 65)  # replace with actual cutpoints if different
for (i in 1:length(cvals)) abline(v = cvals[i], col = "magenta", lty = 2)  # cutpoints

dev.off()  # Close the PNG device

# Clean up
rm(list = ls())

