# STA 380 Group Project

library(tidyverse)
attach(framingham)
glimpse(framingham)

# how many NA values?
sum(is.na(framingham))
sum(is.na(education)) #105
sum(is.na(cigsPerDay)) #29
sum(is.na(BPMeds)) #53
sum(is.na(totChol)) #50
sum(is.na(BMI)) #19
sum(is.na(heartRate)) #1
sum(is.na(glucose)) #388

drop_na(framingham) # prints 3658 rows... maybe this is good enough?

# replace null values
install.packages("DescTools")
library(DescTools)

heart <- replace_na(framingham, list(education = as.integer(Mode(education, na.rm = TRUE)),
                                     BPMeds = as.integer(Mode(BPMeds, na.rm = TRUE)), 
                                     cigsPerDay = as.integer(mean(cigsPerDay, na.rm = TRUE)),
                                     totChol = as.integer(mean(totChol, na.rm = TRUE)),
                                     BMI = mean(BMI, na.rm = TRUE),
                                     heartRate = as.integer(mean(heartRate, na.rm = TRUE)),
                                     glucose = as.integer(mean(glucose, na.rm = TRUE))))
sum(is.na(heart))
attach(heart)

# show distributions of different variables
table(male)
table(education)
table(currentSmoker)
table(BPMeds)
table(prevalentStroke)
table(prevalentHyp)
table(diabetes)
table(TenYearCHD)

hist(age)
hist(cigsPerDay)
hist(totChol)
hist(sysBP)
hist(diaBP)
hist(BMI)
hist(heartRate)
hist(glucose)

# find correlations
cor(currentSmoker, cigsPerDay, use = "complete.obs") # pretty highly correlated
cor(sysBP, diaBP) # pretty highly correlated
cor(diabetes, glucose, use = "complete.obs") # relatively highly correlated
cor(BMI, diabetes, use = "complete.obs") # not correlated

library(corrplot)
cor_matrix <- cor(heart, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")

# simple multiple regression model
glm_all <- glm(TenYearCHD ~ ., data = heart, family = binomial())
summary(glm_all)

# feature selection
glm_sig <- glm(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + sysBP + glucose, 
               data = heart, family = binomial())
summary(glm_sig)

# model evaluation with log loss
baseline <- rep(mean(TenYearCHD), 4240)
predprob_all <- predict(glm_all, type = "response")
predprob_sig <- predict(glm_sig, type = "response")

log_loss <- function(actual, predicted, epsilon = 1e-15) {
  predicted <- pmax(pmin(predicted, 1 - epsilon), epsilon)
  loss <- -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
  return(loss)
}

log_loss(TenYearCHD, baseline)
log_loss(TenYearCHD, predprob_all)
log_loss(TenYearCHD, predprob_sig)


# thoughts...
# maybe transform age in some way? make it nonlinear?
# is it more appropriate to replace values with mean or median?
# age... if someone died in less than 10 years, were they removed from the data? could this affect quality?

