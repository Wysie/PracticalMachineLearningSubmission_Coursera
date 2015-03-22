library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

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

#Split the data. 60% training, 40% testing.
trainingIndex <- createDataPartition(y = trainingData$classe, p=0.6,list=FALSE);
trainingPartition <- trainingData[trainingIndex,];
testingPartition <- trainingData[-trainingIndex,];

#Fix the seed for reproduceability
set.seed(1986)

#Decision Tree Model
dtModel <- rpart(classe ~ ., data=trainingPartition, method="class")
dtPrediction <- predict(dtModel, testingPartition, type = "class")
confusionMatrix(dtPrediction, testingPartition$classe)

#Random Forest Model
rfModel <- randomForest(classe ~. , data=trainingPartition, method="class")
rfPrediction <- predict(rfModel, testingPartition, type = "class")
confusionMatrix(rfPrediction, testingPartition$classe)

#Submission

#Read test file and clean up (identical steps as training file)
testingData <- read.csv(testingFile, na.strings=c("NA", "", " "))
testingData <- testingData[,!apply(is.na(testingData), 2, any)]
testingData <- testingData[,8:ncol(testingData)]

predictTestData <- predict(rfModel, testingData, type="class")
predictTestData

# Write files for submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictTestData)
