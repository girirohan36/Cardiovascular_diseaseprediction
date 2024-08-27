

# Libraries
library(tidyverse)
library(caret)
library(DescTools)
library(glmnet)
library(class)
library(corrplot)
library(MASS)
library(randomForest)
library(gbm)
library(rpart)
library(rpart.plot)
library(pROC)
library(ISLR2)
library(MLmetrics)
library(MLeval)
library(tidyr)
library(GGally)
library(reshape2)
library(tree)

############################################################################

## Exploratory Data Analysis (EDA)

############################################################################

framingham <- read.csv("framingham.csv")
plot_heart <- dplyr::select(framingham,-c("male","currentSmoker","BPMeds","prevalentStroke","prevalentHyp"
                                  ,"diabetes","education")) # selecting continuous variables
boxplot(plot_heart,ylab="values",col="pink")


# Univariate Analysis

# Histogram of continuous variables with density plot overlayed
ggplot(framingham, aes(age)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)
ggplot(framingham, aes(cigsPerDay)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)
ggplot(framingham, aes(totChol)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)
ggplot(framingham, aes(sysBP)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)
ggplot(framingham, aes(diaBP)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)
ggplot(framingham, aes(BMI)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)
ggplot(framingham, aes(heartRate)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)
ggplot(framingham, aes(glucose)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)

# Bar plots for categorical variables
ggplot(framingham)+
  geom_bar(aes(x=TenYearCHD), fill = "#FFABAB") 
ggplot(framingham)+
  geom_bar(aes(x=male), fill = "#0099F8") 
ggplot(framingham)+
  geom_bar(aes(x=education), fill = "#0099F8")
ggplot(framingham)+
  geom_bar(aes(x=currentSmoker), fill = "#0099F8")
ggplot(framingham)+
  geom_bar(aes(x=prevalentStroke), fill = "#0099F8")
ggplot(framingham)+
  geom_bar(aes(x=prevalentHyp), fill = "#0099F8")


### Bivariate Analysis

plot1 <- dplyr::select(framingham, age, cigsPerDay, totChol, sysBP, diaBP, BMI, heartRate, glucose)

# 1. Scatter plots of all pairs of continuous variables

# ggpairs plot combines scatterplots, correlations and univariate density functions into one
# condensed plot
ggpairs(plot1,                 # Data frame
        aes(alpha = 0.5))     # Transparency

# 2. Correlations between all continuous variables
cor_df <- round(cor(plot1, use = "complete.obs"), 2)
#melt the data frame
melted_cormat <- melt(cor_df)
#create correlation heatmap
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 3) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())

############################################################################

## Data Cleaning

############################################################################

# Replace Null Values
heartnw <- replace_na(framingham, list(education = as.integer(Mode(education, na.rm = TRUE)),
                                       BPMeds = as.integer(Mode(BPMeds, na.rm = TRUE)), 
                                       cigsPerDay = as.integer(mean(cigsPerDay, na.rm = TRUE)),
                                       totChol = as.integer(mean(totChol, na.rm = TRUE)),
                                       BMI = mean(BMI, na.rm = TRUE),
                                       heartRate = as.integer(mean(heartRate, na.rm = TRUE)),
                                       glucose = as.integer(mean(glucose, na.rm = TRUE))))
sum(is.na(heartnw))


# Removing Outliers (Winsorizing)
winsorize_data <- function(x, upper = NULL)
{
  if (!is.null(upper)) {
    x[x > upper] <- upper
  }
  return(x)
}


heartw <- heartnw %>% mutate(cigsPerDay = winsorize_data(x=cigsPerDay, upper=quantile(cigsPerDay, 0.99)),
                             totChol = winsorize_data(x=totChol, upper=quantile(totChol, 0.98)),
                             sysBP = winsorize_data(x=sysBP, upper=quantile(sysBP, 0.97)),
                             diaBP = winsorize_data(x=diaBP, upper=quantile(diaBP, 0.98)),
                             BMI = winsorize_data(x=BMI, upper=quantile(BMI, 0.99)),
                             heartRate = winsorize_data(x=heartRate, upper=quantile(heartRate, 0.99)),
                             glucose = winsorize_data(x=glucose, upper=quantile(glucose, 0.98)))

############################################################################

## Feature Engineering

############################################################################

# Breaks to create bins
breaks <- c(0, 200, 239, max(heartnw$totChol))  # the bin boundaries for tot_chol

# Variable interactions
heartnw <- heartnw %>%
 mutate(currentSmoker_cigsPerDay=currentSmoker*cigsPerDay,
        currentSmoker_prevalentHyp = currentSmoker * prevalentHyp,
        age_heartRate = age * heartRate,
        age_BMI = age * BMI,
        age_cigsPerDay = age * cigsPerDay,
        totChol_age = totChol * age,
        glucose_diabetes=diabetes*glucose,
        prevalentStroke_prevalentHyp = prevalentStroke * prevalentHyp,
        totChol_categories= cut(totChol, breaks = breaks, labels=c("Healthy","At-Risk","Dangerous"), include.lowest = TRUE))

############################################################################

## Train-Test Split

############################################################################

set.seed(123)
train_ix = createDataPartition(heartw$TenYearCHD, p = 0.8)
heartw_train = heartw[train_ix$Resample1,]
heartw_test  = heartw[-train_ix$Resample1,]

train_ix = createDataPartition(heartnw$TenYearCHD, p = 0.8)
heartnw_train = heartnw[train_ix$Resample1,]
heartnw_test  = heartnw[-train_ix$Resample1,]

############################################################################

## Logistic Regression

############################################################################

# create base models and use stepwise selection
null = glm(TenYearCHD ~ 1, 
           data=heartw_train, 
           family = binomial)
full = glm(TenYearCHD ~ ., 
           data=heartw_train,
           family = binomial)
summary(full)
sig = glm(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + sysBP,
          data=heartw_train,
          family = binomial)

stepnull = step(null, direction="both", scope = ~(.)^2)
stepfull = step(full, direction="both", scope = ~(.)^2)
stepsig = step(sig, direction="both", scope = ~(.)^2)
summary(stepfull)

# define log loss function
log_loss <- function(actual, predicted, epsilon = 1e-15) {
  predicted <- pmax(pmin(predicted, 1 - epsilon), epsilon)
  loss <- -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
  return(loss)
}

attach(heartw_test)

# ridge regression and LASSO
lasso = cv.glmnet(x = as.matrix(heartw_train[,-16]), 
                  y = heartw_train[,16])
LamL = lasso$lambda.1se
plot(-log(lasso$lambda),sqrt(lasso$cvm),
     main="LASSO CV (k=10)",
     xlab="-log(lambda)",
     ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=-log(LamL),lty=2,col=2,lwd=2)
coef(lasso, s=LamL)
# CV estimated RMSE for 1 SE rule
sqrt(lasso$cvm)[lasso$lambda == LamL]

logloss <- list()

# identify the optimal alpha based on log loss
for (alpha in seq(0, 1, by = 0.01)) {
  ridge = cv.glmnet(x = as.matrix(heartw_train[,-16]), 
                    y = heartw_train[,16],
                    alpha=alpha)
  LamR = ridge$lambda.1se
  plot(-log(ridge$lambda),sqrt(ridge$cvm),
       main="ridge CV (k=10)",
       xlab="-log(lambda)",
       ylab = "RMSE",col=4,type="b",cex.lab=1.2)
  abline(v=-log(LamR),lty=2,col=2,lwd=2)
  coef(ridge, s=LamR)
  sqrt(ridge$cvm)[ridge$lambda == LamR]
  
  plot(coef(ridge, s=LamR), coef(lasso, s=LamL))
  abline(0,1)
  loglossa <- log_loss(TenYearCHD, predict(ridge, as.matrix(heartw_test[,-16]), type = "response"))
  logloss[[paste0("alpha_", alpha)]] <- loglossa
}

print(names(logloss)[which(unlist(logloss) == min(unlist(logloss), na.rm = TRUE))])
print(min(unlist(logloss), na.rm = TRUE))

ridge = cv.glmnet(x = as.matrix(heartw_train[,-16]), 
                  y = heartw_train[,16],
                  alpha=0.34)
LamR = ridge$lambda.1se
plot(-log(ridge$lambda),sqrt(ridge$cvm),
     main="ridge CV (k=10)",
     xlab="-log(lambda)",
     ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=-log(LamR),lty=2,col=2,lwd=2)
coef(ridge, s=LamR)
sqrt(ridge$cvm)[ridge$lambda == LamR]

plot(coef(ridge, s=LamR), coef(lasso, s=LamL))
abline(0,1)

# model evaluation for logistic regression
log_loss(TenYearCHD, rep(mean(TenYearCHD), nrow(heartw_test)))
log_loss(TenYearCHD, predict(null, heartw_test, type = "response"))
log_loss(TenYearCHD, predict(full, heartw_test, type = "response"))
log_loss(TenYearCHD, predict(sig, heartw_test, type = "response"))
log_loss(TenYearCHD, predict(stepnull, heartw_test, type = "response"))
log_loss(TenYearCHD, predict(stepfull, heartw_test, type = "response"))
log_loss(TenYearCHD, predict(stepsig, heartw_test, type = "response"))
log_loss(TenYearCHD, predict(lasso, as.matrix(heartw_test[,-16]), type = "response"))
log_loss(TenYearCHD, predict(ridge, as.matrix(heartw_test[,-16]), type = "response"))

# find the best classification threshold for each model
optimal_threshold <- function(predicted_values, test_data) {
  costs = list()
  for (i in seq(0,1,0.01)) {
    predclass <- ifelse(predict(predicted_values, test_data, type = "response") > i, 1, 0)
    confusion_matrix <- table(predclass, TenYearCHD)
    fp <- confusion_matrix[2]
    fn <- confusion_matrix[3]
    cost <- fp + 5*fn
    costs[[paste0("threshold: ", i)]] <- cost
  }
  print(names(costs)[which(unlist(costs) == min(unlist(costs), na.rm = TRUE))])
  print(min(unlist(costs), na.rm = TRUE))
}

optimal_threshold(full, heartw_test)
optimal_threshold(sig, heartw_test)
optimal_threshold(ridge, as.matrix(heartw_test[,-16]))
optimal_threshold(lasso, as.matrix(heartw_test[,-16]))

# confusion matrix and cost
predclass_full <- ifelse(predict(full, heartw_test, type = "response") > 0.17, 1, 0)
predclass_sig <- ifelse(predict(sig, heartw_test, type = "response") > 0.16, 1, 0)
predclass_ridge <- ifelse(predict(ridge, as.matrix(heartw_test[,-16]), type = "response") > 0.17, 1, 0)
predclass_lasso <- ifelse(predict(lasso, as.matrix(heartw_test[,-16]), type = "response") > 0.16, 1, 0)

table(predclass_full, TenYearCHD)
table(predclass_sig, TenYearCHD)
table(predclass_ridge, TenYearCHD)
table(predclass_lasso, TenYearCHD)

# Plot ROC curve
roc_curve <- roc(TenYearCHD, predict(sig, heartw_test, type = "response"))
plot(roc_curve, col = "blue", main = "ROC Curve for Logistic Regression Model")
auc_value <- auc(roc_obj)
text(0.6, 0.2, paste("AUC =", round(auc_value, 2)), col = "blue")

# evaluation metrics
confusion <- table(predclass_sig, TenYearCHD)
tn <- confusion[1]
fp <- confusion[2]
fn <- confusion[3]
tp <- confusion[4]

precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
specificity <- tn / (tn + fp)

############################################################################

## K-Nearest Neighbors (KNN)

############################################################################

# Convert the TenYearCHD variable to a factor with levels 0 and 1
heartw_train$TenYearCHD <- factor(heartw_train$TenYearCHD, levels = c(0,1))
heartw_test$TenYearCHD <- factor(heartw_test$TenYearCHD, levels = c(0,1))

# Preprocess the training data and then center and scale the variables
preProcValues <- preProcess(heartw_train, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, heartw_train)
testTransformed <- predict(preProcValues, heartw_test)

# Train a k-Nearest Neighbors (kNN) model with cross-validation
knnModel <- train(
  TenYearCHD ~ .,
  data = trainTransformed,
  method = "knn",
  trControl = trainControl(method = "cv"),
  tuneGrid = data.frame(k = c(3,5,7))
)
# Extract the best model based on cross-validation results
best_model <- knn3(
  TenYearCHD ~ .,
  data = trainTransformed,
  k = knnModel$bestTune$k
)

# Make predictions on the test data
predictions <- predict(best_model, testTransformed, type = "class")
probs <- predict(best_model, testTransformed, type = "prob")

# Calculate ROC and AUC
roc_obj <- roc(testTransformed$TenYearCHD, probs[,2])
auc_value <- auc(roc_obj)

# Plot ROC curve
plot.roc(roc_obj, col = "blue", main = "ROC Curve for KNN Classification Model")
abline(a = 0, b = 1, col = "red", lty = 2)
text(0.6, 0.2, paste("AUC =", round(auc_value, 2)), col = "blue")
cat("AUC: ", auc_value, "\n")

# Calculate confusion matrix
confusionmat <- confusionMatrix(predictions, testTransformed$TenYearCHD, positive = "1")
confusionmat

############################################################################

## Decision Tree

############################################################################

heart_tree <- rpart(TenYearCHD ~ ., data = heartnw_train, method = "class")
tree.heart <- tree(heartnw_train$TenYearCHD ~ .,data=heartnw_train)

summary(tree.heart)
plot(tree.heart)
text(tree.heart, pretty=0)

# Plot the decision tree
library(rpart.plot)
rpart.plot(heart_tree, main = "Decision Tree for Framingham Dataset")

# Make predictions on the test set using the trained model
predicted_classes <- predict(heart_tree, heartnw_test, type = "class")

# Calculate accuracy
accuracy <- sum(predicted_classes == heartnw_test$TenYearCHD) / nrow(heartnw_test)
cat("Accuracy on the test set:", accuracy, "\n")

# Confusion Matrix
confusion_matrix <- table(predicted_classes, heartnw_test$TenYearCHD)
confusion_matrix

# Calculate Log Loss
predicted_probabilities <- predict(heart_tree, heartnw_test, type = "prob")
actual_labels <- as.numeric(heartnw_test$TenYearCHD)  # Convert factor levels to 0 and 1

log_loss_value <- log_loss(actual_labels, predicted_probabilities[, 2])
cat("Log Loss:", log_loss_value, "\n")
