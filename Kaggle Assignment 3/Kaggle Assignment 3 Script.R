# Auth: Abhijeet Chopra
# CWID: 50180612 
# Date: 16 Apr 2017
# Prog: League Of Legends
# Desc: Kaggle Assignment # 3
# Meth: Classification Trees using C5.0
# -----------------------------------------

# Setting working directory and import datafiles
#
# NOTE: replace the path between the quotation marks with the 
#       absolute path of the folder (in this case, "League") 
#       containing this R script,the csci527_hw3_test.csv and 
#       csci527_hw3_training.csv files.
setwd("C:/Abhi/TAMUC/#SEM 2/CSCI 527 - Advanced Databases/Assignments/A3 Final")

# Importing data files and storing in variables
dt <- read.csv(file="finalproject_training.csv", header=TRUE, stringsAsFactors = FALSE, nrows = 40000)
tdata <- read.csv(file="finalproject_test.csv", header=TRUE, stringsAsFactors = FALSE)

# PRE-PROCESSING DATA
# -----------------------------------------

dt$winner <- as.factor(dt$winner)


# SHUFFLING ROWS

# nrow(x) returns number of columns in x
# sample(x) shuffles and take sample of size x with/without replacement
train <- sample(nrow(dt), 0.8*nrow(dt))
trainingDt <- dt[train,]
validationDt <- dt[-train,]

trainX <- trainingDt[,names(trainingDt) != "winner"]
trainy <- trainingDt[,c("winner")]

testX <- validationDt[,names(validationDt) != "winner"]
testy <- validationDt[,c("winner")]

# Installing the C50 package ( required for first time installation )
#install.packages("C50")
install.packages(c50)



# DATA MINING
# -----------------------------------------

# BUILDING MODEL

# creating the model using C5.0(~ target, ~ predictor)
model <- C50::C5.0( trainX , trainy )
model <-  C50::C5.0( trainX, trainy, trials=100)

pred <- predict(model, testX, type="prob" )

pred2 <-pred[,2]
head(pred2)


#table(testy, pred, dnn=c("Actual", "Predicted"))



RecordID <- tdata["RecordID"]
#tdata<- tdata[,!(names(tdata)%in%remove)]


# MAKING PREDICTIONS

# predict() function with argument type="prob" gives vector with 
# probabilities for both FALSE and TRUE.


# POST-PROCESSING
# -----------------------------------------

# storing only probabilites for TRUE event in winner variable
final_pred  <- predict(model, tdata, type="prob" )
winner <- final_pred[,2]

head(winner)


rt <- data.frame(RecordID,winner)
write.csv(rt, file = "output.csv", row.names = FALSE)
