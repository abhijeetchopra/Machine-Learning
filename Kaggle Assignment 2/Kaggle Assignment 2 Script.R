# Auth: Abhijeet Chopra
# CWID: 50180612 
# Date: 25 Mar 2017
# Prog: League Of Legends
# Desc: Classification Trees using C5.0
# -----------------------------------------



# Setting working directory and import datafiles
#
# NOTE: replace the path between the quotation marks with the 
#       absolute path of the folder (in this case, "League") 
#       containing this R script,the csci527_hw3_test.csv and 
#       csci527_hw3_training.csv files.
setwd("C:/Users/abhij/Desktop/League")

# Installing the C50 package ( required for first time installation )
install.packages("C50")

# Loading the C50 package
library(C50)



# LOADING DATASETS
# -----------------------------------------

# Importing data files and storing in variables
training <- read.csv("csci527_hw3_training.csv", header = TRUE)
test <- read.csv("csci527_hw3_test.csv", header = TRUE)

# head() function displays the top 6 rows of data
head(training)



# PRE-PROCESSING DATA
# -----------------------------------------

# STORING AS FACTORS

# as.factor() coerces its argument to a factor
# "Storing data as factors insures that the modeling functions will treat 
# such data correctly" [1]
training$championId0 <- as.factor(training$championId0)
training$championId1 <- as.factor(training$championId1)
training$championId2 <- as.factor(training$championId2)
training$championId3 <- as.factor(training$championId3)
training$championId4 <- as.factor(training$championId4)

#
test$championId0 <- as.factor(test$championId0)
test$championId1 <- as.factor(test$championId1)
test$championId2 <- as.factor(test$championId2)
test$championId3 <- as.factor(test$championId3)
test$championId4 <- as.factor(test$championId4)



# SHUFFLING ROWS

# nrow(x) returns number of columns in x
# sample(x) shuffles and take sample of size x with/without replacement
training <- training[ sample( nrow( training ) ), ]



# DATA MINING
# -----------------------------------------

# BUILDING MODEL

# creating the model using C5.0(~ target, ~ predictor)
model <- C5.0( training[,-6], training[,6] )

# boosting the model
model <- C50::C5.0( training[,-6], training[,6], trials=10 )
# NOTE: The Model is ready at this point. Now it's time to predict.



# MAKING PREDICTIONS

# predict() function with argument type="prob" gives vector with 
# probabilities for both FALSE and TRUE.
winner_prob <- predict( model, test, type="prob" )



# POST-PROCESSING
# -----------------------------------------

# storing only probabilites for TRUE event in winner variable
winner <- winner_prob[,-1]

# printing model summary
summary(model)

# analysing/evaluating
table(training[1:46820,6], winner)
#plot(model)

# storing all IDs in variable id
id <- test$id

# storing data frame in variable submit
submit <- data.frame(id, winner)

# write.csv() function creates a csv file from submit
write.csv(submit, file = "output10.csv", row.names = FALSE)

# REFERENCES
# 1. https://www.stat.berkeley.edu/classes/s133/factors.html
# 2. http://connor-johnson.com/2014/08/29/decision-trees-in-r-using-the-c50-package/