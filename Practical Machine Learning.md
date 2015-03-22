---
title: "Prediction Assignment Submission"
author: "Soh Yuan Chin"
date: "March 22, 2015"
output: html_document
---

# Executive Summary
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. 

The goal of this report is to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants to predict the manner in which they did the exercise. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

## Libraries
The following libraries were loaded.

```r
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.1.3
```

```
## Loading required package: lattice
## Loading required package: ggplot2
## Need help? Try the ggplot2 mailing list: http://groups.google.com/group/ggplot2.
```

```r
library(randomForest)
```

```
## Warning: package 'randomForest' was built under R version 3.1.3
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
library(rpart)
```

```
## Warning: package 'rpart' was built under R version 3.1.3
```

```r
library(rpart.plot)
```

```
## Warning: package 'rpart.plot' was built under R version 3.1.3
```

## Data Preparation
The data files and folder are then checked for their existence. If not, the data is downloaded first. In either case, the data is eventually loaded.

```r
#Check if data folder exists, else create it
if (!file.exists("data")) {
  dir.create("data")
}

#Check if training file exists, else download it
trainingFile <- "./data/pml-training.csv"
if (!file.exists(trainingFile)) {
  trainingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
  download.file(trainingUrl, destfile = trainingFile)
}

#Check if testing file exists, else download it
testingFile <- "./data/pml-testing.csv"
if (!file.exists(testingFile)) {
  testingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
  download.file(testingUrl, destfile = testingFile)
}

#Read training file
trainingData <- read.csv(trainingFile, na.strings=c("NA", "", " "))

#Remove columns with NA values
trainingData <- trainingData[,!apply(is.na(trainingData), 2, any)]

#Remove irrelevant columns
trainingData <- trainingData[,8:ncol(trainingData)]
```

## Split the Data for Training and Testing

```r
#Split the data. 60% training, 40% testing.
trainingIndex <- createDataPartition(y = trainingData$classe, p=0.6,list=FALSE);
trainingPartition <- trainingData[trainingIndex,];
testingPartition <- trainingData[-trainingIndex,];
```

## Prepare for Models

```r
#Fix the seed for reproduceability
set.seed(1986)
```

## Decision Tree Model

```r
dtModel <- rpart(classe ~ ., data=trainingPartition, method="class")
dtPrediction <- predict(dtModel, testingPartition, type = "class")
confusionMatrix(dtPrediction, testingPartition$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2022  345   20  113   48
##          B   39  780  106   54  105
##          C   58  223 1136  121  136
##          D   91  103   84  890  108
##          E   22   67   22  108 1045
## 
## Overall Statistics
##                                           
##                Accuracy : 0.7485          
##                  95% CI : (0.7388, 0.7581)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.6806          
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9059  0.51383   0.8304   0.6921   0.7247
## Specificity            0.9063  0.95196   0.9169   0.9412   0.9658
## Pos Pred Value         0.7936  0.71956   0.6786   0.6975   0.8267
## Neg Pred Value         0.9604  0.89086   0.9624   0.9397   0.9397
## Prevalence             0.2845  0.19347   0.1744   0.1639   0.1838
## Detection Rate         0.2577  0.09941   0.1448   0.1134   0.1332
## Detection Prevalence   0.3248  0.13816   0.2134   0.1626   0.1611
## Balanced Accuracy      0.9061  0.73290   0.8737   0.8166   0.8452
```

## Random Forest Model

```r
rfModel <- randomForest(classe ~. , data=trainingPartition, method="class")
rfPrediction <- predict(rfModel, testingPartition, type = "class")
confusionMatrix(rfPrediction, testingPartition$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2227    5    0    0    0
##          B    5 1507    9    0    0
##          C    0    6 1356   11    0
##          D    0    0    3 1272    6
##          E    0    0    0    3 1436
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9939          
##                  95% CI : (0.9919, 0.9955)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9923          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9978   0.9928   0.9912   0.9891   0.9958
## Specificity            0.9991   0.9978   0.9974   0.9986   0.9995
## Pos Pred Value         0.9978   0.9908   0.9876   0.9930   0.9979
## Neg Pred Value         0.9991   0.9983   0.9981   0.9979   0.9991
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2838   0.1921   0.1728   0.1621   0.1830
## Detection Prevalence   0.2845   0.1939   0.1750   0.1633   0.1834
## Balanced Accuracy      0.9984   0.9953   0.9943   0.9939   0.9977
```

The Random Forest Model is select since it yields a higher accuracy (~99%) vs that of the Decision Tree Model (~76%).

##Submission

```r
#Read test file and clean up (identical steps as training file)
testingData <- read.csv(testingFile, na.strings=c("NA", "", " "))
testingData <- testingData[,!apply(is.na(testingData), 2, any)]
testingData <- testingData[,8:ncol(testingData)]

predictTestData <- predict(rfModel, testingData, type="class")
predictTestData
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

```r
# Write files for submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictTestData)
```
