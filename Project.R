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