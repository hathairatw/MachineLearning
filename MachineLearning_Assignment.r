# exactly according to the specification (Class A)
# throwing the elbows to the front (Class B)
# lifting the dumbbell only halfway (Class C)
# lowering the dumbbell only halfway (Class D)
# throwing the hips to the front (Class E).

library(dplyr); library(ggplot2); library(caret)
TrainData <- read.csv("~/pml-training.csv")
TestData <- read.csv("~/pml-testing.csv")

# remove columns with null or NA so 160 variables are now down to 60 variables
TrainData[TrainData == ""] <- NA
TrainData <- TrainData[ , colSums(is.na(TrainData)) == 0]

# create dataset for validation
inBuild <- createDataPartition(y=TrainData$classe,p=0.7,list=FALSE)
validation <- TrainData[-inBuild,]
buildData <- TrainData[inBuild,]

# create training and testing dataset from TrainData
inTrain <- createDataPartition(y=buildData$classe,p=0.7,list=FALSE)
training <- buildData[inTrain,]
testing <- buildData[-inTrain,]

# we predict labelled classification (A to E) which is supervised machine learning
# First choice is building model using random forest 
trainingRF <- subset(training, select = -c(1:7))
mod1 <- train(classe~., method = "rf", data = trainingRF)
mod1$finalModel
pred1 <- predict(mod1, testing)
#qplot(pred1, colour = classe, data = testing)
#pred1v <- predict(mod1, validation)
confusionMatrix(pred1, testing$classe)


# build model using kmeans 
trainingKM <- subset(training, select = -c(1:7))
kmeans1 <- kmeans(subset(trainingKM,select=-c(classe)),centers=5)
trainingKM$clusters <- as.factor(kmeans1$cluster)
table(kmeans1$cluster,trainingKM$classe)
mod2 <- train(clusters~.,data=subset(trainingKM,select=-c(classe)),method="rpart")
table(predict(mod2,trainingKM),trainingKM$classe)
testclusterpred <- predict(mod2,testing)
table(testclusterpred,testing$classe)

