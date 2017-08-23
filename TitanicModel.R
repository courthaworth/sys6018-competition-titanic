# Court Haworth (ach2wd)

library(readr)
library(dplyr)

#Read in training and test data
train <- read_csv("/Users/courthaworth/Downloads/train.csv")
test <-  read_csv("/Users/courthaworth/Downloads/test.csv")

#Drop Name and ticket as they are unique to each passenger and offer no information to the model
train$Name <- NULL
train$Ticket <-  NULL

#Change Sex, Embarked and Cabin to factors
train[,c(4,9:10)] = lapply(train[,c(4,9:10)],factor)
test[,c(4,9:10)] = lapply(test[,c(4,9:10)],factor)


#Build a model that is trained on the most important features
titanic.lg <- glm(Survived~Pclass+Sex+Age, data=train, family = "binomial")
summary(titanic.lg) #You can see all the coefficients have significantly small pvalues
