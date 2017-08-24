# Court Haworth (ach2wd)

library(readr)
library(dplyr)

#set working directory to location of data
setwd("~/Desktop/School/SYS6018/sys6018-competition-titanic")

#Read in training and test data
train <- read_csv("train.csv")
test <-  read_csv("test.csv")

#Drop Name and ticket as they are unique to each passenger and offer no information to the model
train$Name <- NULL
train$Ticket <-  NULL

#Change Sex, Embarked and Cabin to factors
train[,c(4,9:10)] = lapply(train[,c(4,9:10)],factor)
test[,c(4,9:10)] = lapply(test[,c(4,9:10)],factor)

 


#Iteration was used to determine that these three variables contributed most to the model, 
#so they are the only ones kept in the model and code
nrow(train[is.na(train$Sex),])#0
nrow(train[is.na(train$Age),])#177
nrow(train[is.na(train$Pclass),])#0

# need to fill in missing values for age, so I will replace any missing ages with the mean value of the 
#supplied ages. Sex and Pclass have no missing values
train$Age[is.na(train$Age)] <- mean(train$Age[!is.na(train$Age)])  
test$Age[is.na(test$Age)] <- mean(test$Age[!is.na(test$Age)]) 

#Add cross validation
# sub <- sample(1:891,size=446)
# train.tr <-  train[sub,]
# train.val <-  train[-sub,]


#Build a model that is trained on the most important features
titanic.lg <- glm(Survived~Pclass+Sex, data=train.tr, family = "binomial")
summary(titanic.lg) #You can see all the coefficients have significantly small pvalues



predictions <- as.data.frame(test$PassengerId)
predictions$survived = as.vector(predict(titanic.lg,newdata=test, type="response"))
predictions$survived[predictions$survived>=0.5] <- 1
predictions$survived[predictions$survived<0.5] <- 0
write.table(predictions, file = "kaggle-titanic-predictions.csv", row.names=F, col.names=c("PassengerId","Survived"), sep=",") 


