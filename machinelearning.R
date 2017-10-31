library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(knitr)
library(MASS)
library(klaR)

train <- read.csv('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv',header = TRUE, na.strings = c("NA", "",'#DIV/0!'))
test <- read.csv('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv',header = TRUE, na.strings = c("NA", "",'#DIV/0!'))

partition <- createDataPartition(train$classe, list = FALSE, p=0.6)
trainSet <- train[partition,]
testSet <- train[-partition,]
head(trainSet)
dim(trainSet)
head(testSet)
dim(testSet)
summary(testSet)
summary(trainSet)
str(testSet)
str(trainSet)


##clean

set.seed(31459)
trainSet <- trainSet[,colSums(is.na(trainSet)) == 0]
testSet <- testSet[,colSums(is.na(testSet)) == 0]

nearzerovar <- nearZeroVar(trainSet)
trainSet <- trainSet[, -nearzerovar]
testSet <- testSet[, -nearzerovar]

nas <-  sapply(trainSet, function(x) mean(is.na(x))) > 0.95
trainSet <- trainSet[, nas==FALSE]
testSet <- testSet[, nas==FALSE]

trainSet <- trainSet[, -(1:5)]
testSet <- testSet[, -(1:5)]


## build ,odles
ctrl <- trainControl(method = 'cv', number = 3, verboseIter = FALSE)
fit <- train(classe ~ ., preprocess = c("center", "scale"), data = trainSet, method = 'rf', trControl = ctrl)
fit$finalModel
fit2 <-train(classe ~ ., data = trainSet, method = 'rpart')
fit2$finalModel
fit3 <- train(classe ~., data = trainSet, method = "nb")
fit4 <- train(classe ~ ., data=trainSet, method = "gbm",
              trControl = ctrl,
              verbose = FALSE)
predictions <- predict(fit, newdata = testSet)
confusionMatrix(testSet$classe, predictions)
plot(predictions, color = 'red', ylab = 'count', xlab = 'category', title = 'Classification')
###

nearzv <- nearZeroVar(train)
train <- train[, -nearzv]
test <- test[, -nzv]

nas2 <- sapply(train, function(x) mean(is.na(x))) > 0.95
train <- train[, nas2 == FALSE]
test <- test[, nas2 == FALSE]

train <- train[, -(1:5)]
test <- test[, -(1:5)]

ctrl2 <- trainControl(method = 'cv', number = 3, verboseIter = FALSE)
fitFin <- train(classe ~., data = train, method = 'rf', trControl = ctrl2)

pml_write_files <- function(x) {
  n <- length(x)
  for(i in 1:n) {
    filename <- paste0("problem_id_", i, ".txt")
    write.table(x[i], file=filename, quote=F, row.names=F, col.names=F)
  }
}

pml_write_files(as.character(predictions))
