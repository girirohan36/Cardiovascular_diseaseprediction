
#Loading required libraries

library(MASS)
library(caret)
library(randomForest)
library(gbm)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(ISLR2)
library(MLmetrics)
library(MLeval)
library(tidyr)

# Read data from csv file
framingham <- read.csv("C:/Users/grees/OneDrive/Desktop/Summer Term/ML/Group_Project/framingham.csv")

# View dataset
View(framingham) 
head(framingham)

# Get basic summary stats 
summary(framingham) 


attach(framingham)
library(DescTools)

plot_heart <-select(framingham,-c("male","currentSmoker","BPMeds","prevalentStroke","prevalentHyp"
                                   ,"diabetes","education")) # selecting continuous variables
boxplot(plot_heart,ylab="values",col="pink") # Box and whisker plot to visually detect outliers

#Data Wrangling
#Replacing missing values

Heart <- replace_na(framingham, list(education = as.integer(Mode(education, na.rm = TRUE)),
                                     BPMeds = as.integer(Mode(BPMeds, na.rm = TRUE)),
                                     cigsPerDay = as.integer(mean(cigsPerDay, na.rm = TRUE)),
                                     totChol = as.integer(mean(totChol, na.rm = TRUE)),
                                     BMI = mean(BMI, na.rm = TRUE),
                                     heartRate = as.integer(mean(heartRate, na.rm = TRUE)),
                                     glucose = as.integer(mean(glucose, na.rm = TRUE))))
sum(is.na(Heart))

#----------------------------------
#Univariate analysis
#-----------------------------------

# Histogram of continuous variables with density plot overlayed
ggplot(Heart_train, aes(age)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)
ggplot(Heart_train, aes(cigsPerDay)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)
ggplot(Heart_train, aes(totChol)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)
ggplot(Heart_train, aes(sysBP)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)
ggplot(Heart_train, aes(diaBP)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)
ggplot(Heart_train, aes(BMI)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)
ggplot(Heart_train, aes(heartRate)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)
ggplot(Heart_train, aes(glucose)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)

# Bar plots for categorical variables : TenYearCHD(target), male, education, current smoker, 
# prevalent stroke and prevalent hyper tension
ggplot(Heart_train)+
  geom_bar(aes(x=TenYearCHD), fill = "#FFABAB") 
ggplot(Heart_train)+
  geom_bar(aes(x=male), fill = "#0099F8") 
ggplot(Heart_train)+
  geom_bar(aes(x=education), fill = "#0099F8")
ggplot(Heart_train)+
  geom_bar(aes(x=currentSmoker), fill = "#0099F8")
ggplot(Heart_train)+
  geom_bar(aes(x=prevalentStroke), fill = "#0099F8")
ggplot(Heart_train)+
  geom_bar(aes(x=prevalentHyp), fill = "#0099F8")

#Bi-variate analysis

plot1 <- select(Heart_train,c("age","cigsPerDay","totChol","sysBP",
                              "diaBP","BMI","heartRate","glucose"))

# 1. Scatter plots of all pairs of continuous variables

# ggpairs plot combines scatterplots, correlations and univariate density functions into one
# condensed plot
ggpairs(plot1,                 # Data frame
        aes(alpha = 0.5))     # Transparency

# 2. Correlations between all continuous variables
cor_df <- round(cor(plot1), 2)
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


#Compute means of continuous variables by target=TenYearCHD
Heart_train %>%
  group_by(TenYearCHD) %>%
  summarise(across(c("age","cigsPerDay","totChol","sysBP",
                     "diaBP","BMI","heartRate","glucose"), mean))

p <- prop.table(table(Heart_train$TenYearCHD, Heart_train$male), margin=2)
p
ggplot(as.data.frame(p), aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("TenYearCHD distribution by Gender") +
  xlab("Male") +
  ylab("Ten Year CHD")

p <- prop.table(table(Heart_train$TenYearCHD, Heart_train$currentSmoker), margin=2)
p
ggplot(as.data.frame(p), aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("TenYearCHD distribution by current smoker") +
  xlab("Is Current Smoker?") +
  ylab("Ten Year CHD")


p <- prop.table(table(Heart_train$TenYearCHD, Heart_train$prevalentStroke), margin=2)
p
ggplot(as.data.frame(p), aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("TenYearCHD distribution by Prevalent stroke") +
  xlab("Has Prevalent Stroke?") +
  ylab("Ten Year CHD")


p <- prop.table(table(Heart_train$TenYearCHD, Heart_train$prevalentHyp), margin=2)
p
ggplot(as.data.frame(p), aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("TenYearCHD distribution by Prevalent Hypertension") +
  xlab("Has Prevalent Hypertension?") +
  ylab("Ten Year CHD")


p <- prop.table(table(Heart_train$TenYearCHD, Heart_train$diabetes), margin=2)
p
ggplot(as.data.frame(p), aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("TenYearCHD distribution by Diabetes condition") +
  xlab("Has Diabetes?") +
  ylab("Ten Year CHD")


p <- prop.table(table(Heart_train$TenYearCHD, Heart_train$male), margin=1)
p
ggplot(as.data.frame(p), aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Gender distribution by Ten Year CHD") +
  xlab("Ten Year CHD") +
  ylab("Male")

p <- prop.table(table(Heart_train$TenYearCHD, Heart_train$currentSmoker), margin=1)
p
ggplot(as.data.frame(p), aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Current smoker distribution by Ten Year CHD") +
  xlab("Ten Year CHD") +
  ylab("Is Current smoker?")

p <- prop.table(table(Heart_train$TenYearCHD, Heart_train$prevalentStroke), margin=1)
p
ggplot(as.data.frame(p), aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Prevalent stroke distribution by Ten Year CHD") +
  xlab("Ten Year CHD") +
  ylab("Has prevalent stroke?")

p <- prop.table(table(Heart_train$TenYearCHD, Heart_train$prevalentHyp), margin=1)
p
ggplot(as.data.frame(p), aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Prevalent Hypertension distribution by Ten Year CHD") +
  xlab("Ten Year CHD") +
  ylab("Has prevalent hypertension?")

p <- prop.table(table(Heart_train$TenYearCHD, Heart_train$diabetes), margin=1)
ggplot(as.data.frame(p), aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Diabetes distribution by Ten Year CHD") +
  xlab("Ten Year CHD") +
  ylab("Has Diabetes?")


#Features Engineering

attach(Heart)

# Breaks to create bins
breaks <- c(0, 200, 239, max(Heart$totChol))  # the bin boundaries for tot_chol

#Variable interactions
Heart <- Heart %>%
  mutate(currentSmoker_cigsPerDay=currentSmoker*cigsPerDay,
         currentSmoker_prevalentHyp = currentSmoker * prevalentHyp,
         age_heartRate = age * heartRate,
         age_BMI = age * BMI,
         age_cigsPerDay = age * cigsPerDay,
         totChol_age = totChol * age,
         glucose_diabetes=diabetes*glucose,
         prevalentStroke_prevalentHyp = prevalentStroke * prevalentHyp,
         totChol_categories= cut(totChol, breaks = breaks, labels=c("Healthy","At-Risk","Dangerous"), include.lowest = TRUE),)



#Labeling Levels
Heart$TenYearCHD = factor(Heart$TenYearCHD, 
                          levels=c(1, 0),labels = c("Yes","No"))


set.seed(123) # To replicate

# Hold out 20% of the data as a final validation set
train_ix = createDataPartition(Heart$TenYearCHD,
                               p = 0.8)

Heart_train = Heart[train_ix$Resample1,]
Heart_test  = Heart[-train_ix$Resample1,]

# Note that caret used stratified sampling to preserve
# the balance of Y/N:
table(Heart$TenYearCHD[train_ix$Resample1]) %>% 
  prop.table
table(Heart$TenYearCHD[-train_ix$Resample1]) %>% 
  prop.table

###########################################################################
# Setup cross-validation
###########################################################################

# Number of folds
kcv = 10
cv_folds = createFolds(Heart_train$TenYearCHD,
                       k = kcv)

###########################################################################
# RANDOM FOREST
###########################################################################

my_summary = function(data, lev = NULL, model = NULL) {
  default = defaultSummary(data, lev, model)
  twoclass = twoClassSummary(data, lev, model)
  # Converting to TPR and FPR instead of sens/spec
  twoclass[3] = 1-twoclass[3]
  names(twoclass) = c("AUC_ROC", "TPR", "FPR")
  logloss = mnLogLoss(data, lev, model)
  c(default,twoclass, logloss)
}

fit_control <- trainControl(
  method = "cv",
  indexOut = cv_folds,
  # Save predicted probabilities, not just classifications
  classProbs = TRUE,
  # Save all the holdout predictions, to summarize and plot
  savePredictions = TRUE,
  summaryFunction = my_summary,
  selectionFunction="oneSE")

rf_grid <- expand.grid(mtry = c(2,3,4,5)) #No. of predictors

rffit <- train(TenYearCHD ~ ., data = Heart_train, 
               method = "rf", 
               trControl = fit_control,
               tuneGrid = rf_grid,
               metric = "logLoss",  
               verbose = FALSE)

print(rffit)
plot(rffit)

# Extracting performance summaries
confusionMatrix(rffit)

# Thresholding for class probabilities
rffit_res <- thresholder(rffit, 
                         threshold = seq(0, 1, by = 0.01), 
                         final = TRUE)

optim_J <- rffit_res[which.max(rffit_res$J),]


# ROC curve
ggplot(aes(x = 1 - Specificity, y = Sensitivity), data = rffit_res) + 
  geom_line() + 
  ylab("TPR (Sensitivity)") + 
  xlab("FPR (1-Specificity)") + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dotted') +
  geom_segment(aes(x = 1 - Specificity, xend = 1 - Specificity, y = 1 - Specificity, yend = Sensitivity), color = 'darkred', data = optim_J) + 
  theme_bw()

# PR curve
ggplot(aes(x = Recall, y = Precision), data = rffit_res) + 
  geom_point() + 
  geom_line() + 
  ylab("Precision") + 
  xlab("Recall (TPR)") + 
  geom_point(aes(x = Recall, y = Precision), color = 'darkred', data = optim_J) + 
  theme_bw()

# Lift curve

best_pars <- rffit$bestTune
best_preds <- rffit$pred %>% filter(mtry == best_pars$mtry)

rf_lift <- caret::lift(obs ~ Yes, data = best_preds)

ggplot(rf_lift) + 
  geom_abline(slope = 1, linetype = 'dotted') +
  xlim(c(0, 10)) + 
  theme_bw()

# Calibration plot

rf_cal <- caret::calibration(obs ~ Yes, data = best_preds, cuts = 7)
ggplot(rf_cal) + theme_bw()

#### Holdout set results

test_probs <- predict(rffit, newdata = Heart_test, type = "prob")


get_metrics <- function(threshold, test_probs, true_class, 
                        pos_label, neg_label) {
  pc <- factor(ifelse(test_probs[, pos_label] > threshold, pos_label, neg_label), levels = c(pos_label, neg_label))
  test_set <- data.frame(obs = true_class, pred = pc, test_probs)
  my_summary(test_set, lev = c(pos_label, neg_label))
}

get_metrics(0.27, test_probs, Heart_test$TenYearCHD, "Yes", "No")

thr_seq <- seq(0, 1, length.out = 500)
metrics <- lapply(thr_seq, function(x) get_metrics(x, test_probs, Heart_test$TenYearCHD, "Yes", "No"))
metrics_df <- data.frame(do.call(rbind, metrics))

#confusion matrix
test_preds_optim <- factor(ifelse(test_probs[,1] > 0.2, "Yes","No"))
confusionMatrix(test_preds_optim, Heart_test$TenYearCHD)

# ROC curve

ggplot(aes(x = FPR, y = TPR), data = metrics_df) + 
  geom_line() +
  ylab("TPR (Sensitivity)") + 
  xlab("FPR (1-Specificity)") + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dotted') +
  annotate("text", x = 0.75, y = 0.25, 
           label = paste("AUC:", round(metrics_df$AUC_ROC[1], 2))) +
  theme_bw()

# Lift

gbm_oos_lift = caret::lift(Heart_test$TenYearCHD~test_probs[,1])

ggplot(gbm_oos_lift) + 
  geom_abline(slope=1, linetype='dotted') +
  xlim(c(0, 100)) + 
  theme_bw()

# Calibration

gbm_cal = caret::calibration(Heart_test$TenYearCHD~test_probs[,1], 
                             data=best_preds, cuts=7)
ggplot(gbm_cal) + theme_bw()

names(rffit)

###########################################################################
# BOOSTING
###########################################################################

my_summary = function(data, lev = NULL, model = NULL) {
  default = defaultSummary(data, lev, model)
  twoclass = twoClassSummary(data, lev, model)
  # Converting to TPR and FPR instead of sens/spec
  twoclass[3] = 1-twoclass[3]
  names(twoclass) = c("AUC_ROC", "TPR", "FPR")
  logloss = mnLogLoss(data, lev, model)
  c(default,twoclass, logloss)
}

fit_control <- trainControl(
  method = "cv",
  indexOut = cv_folds,
  # Save predicted probabilities, not just classifications
  classProbs = TRUE,
  # Save all the holdout predictions, to summarize and plot
  savePredictions = TRUE,
  summaryFunction = my_summary,
  selectionFunction="oneSE")

gbm_grid <-  expand.grid(interaction.depth = c(1, 3, 5), 
                         n.trees = c(100, 500, 750, 1000), 
                         shrinkage = c(0.1,0.2),
                         n.minobsinnode = 10)

gbmfit <- train(TenYearCHD ~ ., data = Heart_train, 
                method = "gbm", 
                trControl = fit_control,
                tuneGrid = gbm_grid,
                metric = "logLoss",
                verbose = FALSE)

print(gbmfit)
plot(gbmfit)
summary(gbmfit) # get variable importance

# Extracting performance summaries
# Confusion matrix as proportions, not counts, since 
# the test dataset varies across folds
# These are CV estimates of error rates/accuracy using a *default* cutoff
# to classify cases

confusionMatrix(gbmfit)

thresholder(gbmfit, 
            threshold = 0.5, 
            final = TRUE,
            statistics = c("Sensitivity",
                           "Specificity"))

gbmfit_res = thresholder(gbmfit, 
                         threshold = seq(0, 1, by = 0.01), 
                         final = TRUE)

# How do metrics vary with the threshold?
pldf = gbmfit_res %>%
  mutate(TPR=Sensitivity, FPR = 1-Specificity, FNR = 1-Sensitivity) %>%
  dplyr::select(-c(n.trees, interaction.depth, shrinkage, n.minobsinnode)) %>%
  pivot_longer(-prob_threshold) 

ggplot(aes(x=prob_threshold, y=value, color=name), 
       data=pldf %>% filter(name %in% c("TPR", "FPR"))) + 
  geom_line() 

ggplot(aes(x=prob_threshold, y=value, color=name), 
       data=pldf %>% filter(name %in% c("FNR", "FPR"))) + 
  geom_line() 

# How do we get points on the ROC curve? One (TPR, FPR) pair for each threshold

thres = 0.5
tp = gbmfit_res %>% 
  dplyr::filter(prob_threshold==thres) %>% 
  dplyr::select(prob_threshold, Sensitivity, Specificity) %>%
  mutate(TPR=Sensitivity, FPR = 1-Specificity)

ggplot(aes(x=prob_threshold, y=value, color=name), 
       data=pldf %>% filter(name %in% c("TPR", "FPR"))) + 
  geom_line() + 
  geom_vline(xintercept=thres, lty=2) + 
  geom_point(aes(x=prob_threshold, y=TPR, color=NULL), data=tp) + 
  geom_point(aes(x=prob_threshold, y=FPR, color=NULL), data=tp) 

# ROC curve

optim_J = gbmfit_res[which.max(gbmfit_res$J),]

ggplot(aes(x=prob_threshold, y=J), 
       data=gbmfit_res) + 
  geom_line() + 
  geom_vline(aes(xintercept=optim_J$prob_threshold), lty=2)

ggplot(aes(x=1-Specificity, y=Sensitivity), data=gbmfit_res) + 
  geom_line() + 
  ylab("TPR (Sensitivity)") + 
  xlab("FPR (1-Specificity)") + 
  geom_abline(intercept=0, slope=1, linetype='dotted') +
  geom_segment(aes(x=1-Specificity, xend=1-Specificity, y=1-Specificity, yend=Sensitivity), color='darkred', data=optim_J) + 
  theme_bw()

# PR curve

ggplot(aes(x=prob_threshold, y=value, color=name), 
       data=pldf %>% filter(name %in% c("Precision", "Recall"))) + 
  geom_line() 

ggplot(aes(x=Recall, y=Precision), data=gbmfit_res) + 
  geom_point() + 
  geom_line() + 
  ylab("Precision") + 
  xlab("Recall (TPR)") + 
  geom_point(aes(x=Recall, y=Precision), color='darkred', data=optim_J) + 
  theme_bw()

# Lift curve

# Extract predicted probs for best-fitting model
# For each observation it's predicted prob is computed when its
# fold is the testing/holdout dataset during CV
best_pars = gbmfit$bestTune
best_preds = gbmfit$pred %>% filter(n.trees==best_pars$n.trees, 
                                    interaction.depth==best_pars$interaction.depth)

gbm_lift = caret::lift(obs~Yes, data=best_preds)

ggplot(gbm_lift) + 
  geom_abline(slope=1, linetype='dotted') +
  xlim(c(0, 100)) + 
  theme_bw() + xlim(0,10)

# Calibration plot

gbm_cal = caret::calibration(obs~Yes, data=best_preds, cuts=7)
ggplot(gbm_cal) + theme_bw()

############################################################################
# Holdout set results
############################################################################

test_probs = predict(gbmfit, newdata=Heart_test, type="prob")

get_metrics = function(threshold, test_probs, true_class, 
                       pos_label, neg_label) {
  # Get class predictions
  pc = factor(ifelse(test_probs[pos_label]>threshold, pos_label, neg_label), levels=c(pos_label, neg_label))
  test_set = data.frame(obs = true_class, pred = pc, test_probs)
  my_summary(test_set, lev=c(pos_label, neg_label))
}

# Get metrics for a given threshold
get_metrics(0.13, test_probs, Heart_test$TenYearCHD, "Yes", "No")

# Compute metrics on test data using a grid of thresholds
thr_seq = seq(0, 1, length.out=500)
metrics = lapply(thr_seq, function(x) get_metrics(x, test_probs, Heart_test$TenYearCHD, "Yes", "No"))
metrics_df = data.frame(do.call(rbind, metrics))


# ROC curve

ggplot(aes(x=FPR, y=TPR), data=metrics_df) + 
  geom_line() +
  ylab("TPR (Sensitivity)") + 
  xlab("FPR (1-Specificity)") + 
  geom_abline(intercept=0, slope=1, linetype='dotted') +
  annotate("text", x=0.75, y=0.25, 
           label=paste("AUC:",round(metrics_df$AUC_ROC[1], 2))) +
  theme_bw()

#confusion matrix
test_preds_optim <- factor(ifelse(test_probs[,1] > 0.28, "Yes","No"))
confusionMatrix(test_preds_optim, Heart_test$TenYearCHD)


# Lift

gbm_oos_lift = caret::lift(Heart_test$TenYearCHD~test_probs[,1])

ggplot(gbm_oos_lift) + 
  geom_abline(slope=1, linetype='dotted') +
  xlim(c(0, 100)) + 
  theme_bw()

# Calibration

gbm_cal = caret::calibration(Heart_test$TenYearCHD~test_probs[,1], 
                             data=best_preds, cuts=7)
ggplot(gbm_cal) + theme_bw()

