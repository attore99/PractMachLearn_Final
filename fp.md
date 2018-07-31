---
title: "Practical Machine Learning - final project"
author: "Francesco Andreini"
date: "31 July 2018"
output: html_document
---

#Introduction: data retrieval and cleaning
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement ??? a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har, to which we are grateful for the data available. 

Firstly, we load all the libraries we need and afterwe retrieve data for training and data set. After that, we partition the training data in training and test set.  The testing data will be used as validation set.

```{r setup}
library(readr)
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(knitr)

set.seed(12345)
pml_training <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),header=T)
validation <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),header=T)

Training <- createDataPartition(pml_training$classe, p=0.7, list=FALSE)
train <- pml_training[Training, ]
test <- pml_training[-Training, ]

dim(train)
dim(test)
dim(validation)
```
We then remove (near) zero variance predictors and/or predictors that are have both of the following characteristics: very few unique values relative to the number of samples and ratio of the frequency of the most common value to the frequency of the second most common value very large. 
```{r nearzero}
nzv <- nearZeroVar(train, saveMetrics=TRUE)
train <- train[,nzv$nzv==FALSE]
test <- test[,nzv$nzv==FALSE]
validation <- validation[,nzv$nzv==FALSE]
train<-train[,-1]
test<-test[,-1]
validation<-validation[,-1]

dim(train)
dim(test)
dim(validation)
```
Finally, we delete all columns with more than 70% of missing values from the training, the test and the validation sets.
```{r null}
columns_to_excl<-c()
for (x in 1:ncol(train)){
  if (sum(is.na(train[,x]))/nrow(train)<=0.7 || colnames(train)[x]=='classe'){
    columns_to_excl<-c(columns_to_excl,x)
  }
}
train<-train[,columns_to_excl]
test<-test[,columns_to_excl]
validation<-validation[,columns_to_excl]

dim(train)
dim(test)
dim(validation)
```

#Predictions
##Random Forests
We firstly try to build a prediction model by using random forest.
```{r rf}
contrRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
RF <- train(classe ~ ., data=train, method="rf", trControl=contrRF)
RF$finalModel
predict1 <- predict(RF, newdata=test)
forest <- confusionMatrix(predict1, test$classe)
forest
plot(RF)
plot(forest$table, col = forest$byClass, main = paste("Random Forest: accuracy is ", forest$overall['Accuracy']))
```
Accuracy is very high and it is 0.9997 about. It means that expected out-of-sample error is practically null.

##Decision tree
```{r dt}
decisionTree <- rpart(classe ~ ., data=train, method="class")
prp(decisionTree)
predict2 <- predict(decisionTree, test, type = "class")
tree <- confusionMatrix(predict2, test$classe)
tree
plot(tree$table, col = tree$byClass, 
     main = paste("Decision Tree: accuracy is ", tree$overall['Accuracy']))
```
The accuracy is lower than the previous case. It is 0.87, so we have an expected error of 0.13 about.

##Boosted regression
```{r boo}
contrBR <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
BR<- train(classe ~ ., data=train, method = "gbm", trControl = contrBR, verbose = FALSE)
BR$finalModel
print(BR)
predict3 <- predict(BR, newdata=test)
boosted <- confusionMatrix(predict3, test$classe)
boosted
```
By combining various predicting model, we now use a boosted regression model. This can be very useful to get further accuracy by combining weak predictors. However, accuracy is 0.9947 - predictors used were not so weak, indeed!
We will then opt for random forest.

#Predictions
Here you will find the predictions on the 20 cases from validation set.
```{r pred}
Results <- predict(RF, newdata=validation)
Results
```