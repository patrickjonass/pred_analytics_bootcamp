#######################################
####### PCA AND FACTOR ANALYSIS  ######
#######################################

setwd("C:\\Users\\patrick.j.a.tibayan\\Downloads\\Trainings\\Predictive Analytics Bootcamp\\Second Checkpoint")

cerealDF <- read.csv("cereal.csv")
cerealDF

rownames(cerealDF) <- cerealDF$Cereals
cerealDF_std <- as.data.frame(scale(cerealDF[,c(2:ncol(cerealDF))], center = TRUE,scale = TRUE))

cerealCovMat <- cov(cerealDF_std)

cerealCovMat.eigen <- eigen(cerealCovMat)
eigVal <- as.data.frame(cerealCovMat.eigen$values)

for (i in 1:nrow(eigVal)){
  eigVal$`ExplainedVar`[i] <- eigVal$`cerealCovMat.eigen$values`[i]/sum(eigVal$`cerealCovMat.eigen$values`)
  cumVal=0
  count=i
  while (count > 0) {
    cumVal <- cumVal + eigVal$ExplainedVar[count]
    count = count - 1
  }
  eigVal$`CumExplained`[i] <- cumVal
}

eigVal

plot(eigVal$ExplainedVar, type="b", col="red")

factanal(cerealDF[,c(2:ncol(cerealDF))], factor = 5,  rotation ="varimax", scores = "regression")
factanal(cerealDF[,c(2:ncol(cerealDF))], factor = 5,  rotation ="promax", scores = "regression")


#######################################
########      REGRESSION     ##########
#######################################
library('sqldf')
library('MASS')
library(pROC)
library(tidyverse)
library(broom)
library(caret)
library(spatstat)
library(car)

bankDF <- read.csv("C:/Users/patrick.j.a.tibayan/Downloads/Trainings/Predictive Analytics Bootcamp/Second Checkpoint/bank.csv",)

bankDF<-sqldf("SELECT [age], [job], [marital], [education], [default], [housing], [loan], [contact], [duration], [campaign], [previous], [poutcome], [y] FROM bankDF")

bankDF$y <- ifelse(bankDF$y == "yes", 1, 0)
bankDF$y <- factor(bankDF$y, levels = c(0, 1))

bankDF$duration <- log(bankDF$duration)

summary(bankDF)
str(bankDF)

set.seed(123)
ind<-sample(nrow(bankDF),0.8*nrow(bankDF))
trainDF <- subset(bankDF[ind,])
testDF <- subset(bankDF[-ind,])

table(trainDF$y)

set.seed(123)
down_train1 <- downSample(x = trainDF[,-13],
                         y = trainDF$y)
table(down_train1$Class)

logitmod <- glm(Class ~ ., family = "binomial", data=down_train1)
summary(logitmod)

stepAIC(logitmod, details = TRUE)

logitmod <- glm(Class ~ education + campaign + housing + loan + contact + poutcome + duration , family = "binomial", data=down_train1)

car::vif(logitmod)

plot(logitmod, which = 4, id.n = 3)

down_train1$pred <- predict(logitmod, newdata = down_train1, type = "response")
down_train1$logofodds <- log(down_train1$pred / (1 - log(down_train1$pred)))

plot(down_train1$education, down_train1$logofodds)
plot(down_train1$campaign, down_train1$logofodds)
plot(down_train1$housing, down_train1$logofodds)
plot(down_train1$loan, down_train1$logofodds)
plot(down_train1$contact, down_train1$logofodds)
plot(down_train1$poutcome, down_train1$logofodds)
plot(down_train1$duration, down_train1$logofodds)

pred_test <- predict(logitmod, newdata = testDF, type = "response")
pred_test

par(pty="s")
roc_curve <- roc(predictor = pred_test, response = testDF$y)
plot(roc_curve,legacy.axes = TRUE, xlab = "False Positive Percentage", ylab = "True Positive Percentage")
auc(roc_curve)

roc.info <- roc(predictor = pred_test,
                response = test$Class,
                legacy.axes = TRUE)

roc.df <- data.frame(tpp = roc.info$sensitivities*100, 
                     fpp = (1- roc.info$specificities)*100, 
                     thresholds = roc.info$thresholds)

y_pred_num <- ifelse(pred_test > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testDF$y

confusion_matrix <- table(y_act, y_pred)

Accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2]) / (confusion_matrix[1,1]+ confusion_matrix[1,2] +confusion_matrix[2,1]+confusion_matrix[2,2])
Sensitivity <- confusion_matrix[2,2] / (confusion_matrix[2,1]+confusion_matrix[2,2])
Specificity <- confusion_matrix[1,1] / (confusion_matrix[1,1]+confusion_matrix[1,2])
