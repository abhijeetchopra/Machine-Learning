# Abhijeet Chopra - 19 Feb 2017
# Titanic
# Reference from guide available at http://trevorstephens.com/

# Set working directory and import datafiles
# NOTE: if the below four commands don't work, replace the "./" 
#       with the absolute path of the folder containing this R script, 
#       the test.csv and train.csv files.
setwd("./")
library(readr)
train <- read_csv("./train.csv")
test <- read_csv("./test.csv")


# view imported datafile
View(train)

# to look at the structure of the dataframe
str(train)

# adding survived column to the test.csv with each cell in 418 rows having value zero
test$Survived <- rep(0, 418)

# extracting two columns from the test dataframe and 
# storing them in a new container, and then sending it to an output file
# NOTE: the csv headers are missing right now, change code later
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

# proportion table command to do two-way comparison
# prop.table(table(train$Sex, train$Survived))
# above command would give females survived/total passengers
# below command would give females survived/total females
# note the argument '1' in the end of the command below
prop.table(table(train$Sex, train$Survived),1)

# 
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

# creating new variable "Child" for passengers with age < 18 
train$Child <- 0
train$Child[train$Age < 18] <- 1

test$Child <- 0
test$Child[test$Age < 18] <- 1

# aggregate function over Child and Sex on Survived
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)

# total number of people in each subset
aggregate(Survived ~ Child + Sex, data=train, FUN=length)

# to calculate proportions
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

### Recursive Partitioning using rpart
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")

# installing built-in packages for graphics
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# predict
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

      # Just for testing, actually results in worse result so don't try
      # removing constraints from rpart
      # fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
      #              data=train,
      #              method="class", 
      #              control=rpart.control(minsplit=2, cp=0))
      # fancyRpartPlot(fit)

# combining test and train
test$Survived <- NA
combi <- rbind(train, test)

# type casting back to text string
combi$Name <- as.character(combi$Name)
combi$Name[1]

# string split function to break original name over these two symbols(,.)
strsplit(combi$Name[1], split='[,.]')

# We feed sapply our vector of names and our function that we just came up with. 
# It runs through the rows of the vector of names, and sends each name to the 
# function. The results of all these string splits are all combined up into a 
# vector as output from the sapply function, which we then store to a new column
# in our original dataframe, called Title.
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

# removing spaces
combi$Title <- sub(' ', '', combi$Title)

# viewing results
table(combi$Title)

# combining titles
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# changing variable type back to factor
combi$Title <- factor(combi$Title)

# combining Sibsp and Parch into FamilySize i.e. adding siblings, spouses, parents, children and themselves
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# extracting family surnames
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

# using paste function to bring two strings together
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

# assigning any family of size 2 or less, the value small
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

# viewing combined family IDs
table(combi$FamilyID)

# storing the data frame in variable famIDs
famIDs <- data.frame(table(combi$FamilyID))

# filtering only values 2 or less
famIDs <- famIDs[famIDs$Freq <= 2,]

# assigning small to families 2 or less as stored in famIDs above
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]

# rpart function (recursive partitioning) function is the package 
# in R programming language that implements the decision tree algorithm
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")

# predict function 
Prediction <- predict(fit, test, OOB=TRUE, type = "response")

# storing data frame in variable submit
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

# write.csv function creates a csv file from submit
write.csv(submit, file = "abhi_sub2.csv", row.names = FALSE)