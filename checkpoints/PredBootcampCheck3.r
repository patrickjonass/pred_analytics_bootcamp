#######################################

####       THIRD CHECKPOINT        ####

#######################################

library(sqldf)
library(caret)

setwd("C:\\Users\\patrick.j.a.tibayan\\Downloads\\Trainings\\Predictive Analytics Bootcamp\\Third Checkpoint")

liverDF <- read.csv("Liver Data.csv")

sapply(liverDF, function(x)sum(is.na(x)))
liverDF <- sqldf("SELECT * FROM liverDF WHERE [Albumin_and_Globulin_Ratio] IS NOT NULL")

set.seed(777)
intrain <- createDataPartition(y = liverDF$Dataset, p= 0.7, list = FALSE)
training <- liverDF[intrain,]
testing <- liverDF[-intrain,]

dim(training)
dim(testing)

summary(liverDF)

training[["Dataset"]] <- factor(training[["Dataset"]])

fitcontrol <- trainControl(method = "repeatedcv", number = 10, repeats = 5, sampling = "smote")

### KNN ###
knnModel <- train(Dataset ~., data = training, method = "knn",
                    trControl=fitcontrol,
                    preProcess = c("center", "scale"))
knnModel

test_predKnn <- predict(knnModel, newdata = testing)
test_predKnn


caret::confusionMatrix(table (test_predKnn, testing$Dataset))

### SVM ###
svmModel <- train(Dataset ~., data = training, method = "svmLinear",
                    trControl=fitcontrol,
                    preProcess = c("center", "scale"))
svmModel

test_predSVM <- predict(svmModel, newdata = testing)
test_predSVM


caret::confusionMatrix(table (test_predSVM, testing$Dataset))

### NAIVE ###
nbModel <- train(Dataset ~., data = training, method = "nb",
                     trControl=fitcontrol,
                     preProcess = c("center", "scale"))
nbModel


test_predNB <- predict(nbModel, newdata = testing)

caret::confusionMatrix(table(test_predNB, testing$Dataset))


### RF ###
rfModel <- train(Dataset~., data = training, method = 'rf',
                 trControl = fitcontrol,
                 preProcess = c("center", "scale"))
rfModel

test_predRF <- predict(rfModel, newdata = testing)

caret::confusionMatrix(table(test_predRF, testing$Dataset))
