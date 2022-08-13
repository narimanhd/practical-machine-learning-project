library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(corrplot)
trainurl<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testurl<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

training<-read.csv(url(trainurl))
testing<-read.csv(url(testurl))

inTrain<-createDataPartition(y=training$classe,p=0.7,list=FALSE)

Train<-training[inTrain,]
Test<-training[-inTrain,]
dim(Train)
dim(Test)

NZV <- nearZeroVar(Train)
Train <- Train[, -NZV]
Test  <- Test[, -NZV]

dim(Train)
dim(Test)
# remove variables that are mostly NAs
ANA    <- sapply(Train, function(x) mean(is.na(x))) > 0.95
Train <- Train[, ANA==FALSE]
Test  <- Test[, ANA==FALSE]
dim(Train)
dim(Test)

# remove identification only variables (columns 1 to 5)
Train <- Train[, -(1:5)]
Test  <- Test[, -(1:5)]
dim(Train)
dim(Test)

corMatrix <- cor(Train[, -54])
corrplot(corMatrix, order = "FPC", method = "color", type = "lower", tl.cex = 0.8, tl.col = rgb(0, 0, 0))


# model fit
set.seed(12345)
controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
modFitRandForest <- train(classe ~ ., data=TrainSet, method="rf",
                          trControl=controlRF)
modFitRandForest$finalModel

# prediction on Test dataset
predictRandForest <- predict(modFitRandForest, newdata=TestSet)
confMatRandForest <- confusionMatrix(predictRandForest, TestSet$classe)
confMatRandForest

# plot matrix results
plot(confMatRandForest$table, col = confMatRandForest$byClass, 
     main = paste("Random Forest - Accuracy =",
                  round(confMatRandForest$overall['Accuracy'], 4)))

# model fit
set.seed(12345)
modFitDecTree <- rpart(classe ~ ., data=TrainSet, method="class")
fancyRpartPlot(modFitDecTree)

# prediction on Test dataset
predictDecTree <- predict(modFitDecTree, newdata=TestSet, type="class")
confMatDecTree <- confusionMatrix(predictDecTree, TestSet$classe)
confMatDecTree


# plot matrix results
plot(confMatDecTree$table, col = confMatDecTree$byClass, 
     main = paste("Decision Tree - Accuracy =",
                  round(confMatDecTree$overall['Accuracy'], 4)))

# model fit
set.seed(12345)
controlGBM <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
modFitGBM  <- train(classe ~ ., data=TrainSet, method = "gbm",
                    trControl = controlGBM, verbose = FALSE)
modFitGBM$finalModel

# prediction on Test dataset
predictGBM <- predict(modFitGBM, newdata=TestSet)
confMatGBM <- confusionMatrix(predictGBM, TestSet$classe)
confMatGBM

# plot matrix results
plot(confMatGBM$table, col = confMatGBM$byClass, 
     main = paste("GBM - Accuracy =", round(confMatGBM$overall['Accuracy'], 4)))