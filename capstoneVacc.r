library(sqldf)
library(ggplot2)
library(reshape2)
library(olsrr)
library(caret)
library(MASS)
library(mice)
library(pROC)
library(h2o)
library(nnet)

setwd("C:/Users/patrick.j.a.tibayan/Downloads/Trainings/Predictive Analytics Bootcamp/Capstone/Vaccine")
vlabel <- read.csv("Vaccine_labels.csv", stringsAsFactors = FALSE)
vfeatures <- read.csv("Vaccine_features.csv", stringsAsFactors = FALSE)

vlabel <- vlabel[c("respondent_id","seasonal_vaccine")]
vDF <- merge(vfeatures,vlabel,by="respondent_id")
vDF <- vDF[,-1]

##### RAW DATA TO TECHNICALLY CORRECT DATA
str(vDF)

vDF$education[which(vDF$education=="")] <- NA
vDF$income_poverty[which(vDF$income_poverty=="")] <- NA
vDF$marital_status[which(vDF$marital_status=="")] <- NA
vDF$rent_or_own[which(vDF$rent_or_own=="")] <- NA
vDF$employment_status[which(vDF$employment_status=="")] <- NA
vDF$employment_industry[which(vDF$employment_industry=="")] <- NA
vDF$employment_occupation[which(vDF$employment_occupation=="")] <- NA
vDF[,grep("household",colnames(vDF),invert = TRUE)] <- lapply(vDF[,grep("household",colnames(vDF),invert = TRUE)],as.factor)

str(vDF)

##### TECHNICALLY CORRECT DATA TO CONSISTENT DATA

##check NA
apply(vDF,2,function(x) any(is.na(x)))
apply(vDF,2,function(x) sum(is.na(x)))

pMiss <- function(x){(sum(is.na(x))/length(x))*100}
apply(vDF,2,pMiss)

vDF <- vDF[ , -which(names(vDF) %in% c("health_insurance","employment_industry","employment_occupation"))]

pMiss <- function(x){(sum(is.na(x))/length(x))*100}
apply(vDF,2,pMiss)

init <- mice(vDF,maxit = 0)
meth <- init$method
predM <- init$predictorMatrix

meth[c("age_group")]=""
meth[c("race")]=""
meth[c("sex")]=""
meth[c("hhs_geo_region")]=""
meth[c("census_msa")]=""
meth[c("seasonal_vaccine")]=""

meth[c("h1n1_concern")] = "polr"
meth[c("h1n1_knowledge")] = "polr"
meth[c("behavioral_antiviral_meds")] = "logreg"
meth[c("behavioral_avoidance")] = "logreg"
meth[c("behavioral_face_mask")] = "logreg"
meth[c("behavioral_wash_hands")] = "logreg"
meth[c("behavioral_large_gatherings")] = "logreg"
meth[c("behavioral_outside_home")] = "logreg"
meth[c("behavioral_touch_face")] = "logreg"
meth[c("doctor_recc_h1n1")] = "logreg"
meth[c("doctor_recc_seasonal")] = "logreg"
meth[c("chronic_med_condition")] = "logreg"
meth[c("child_under_6_months")] = "logreg"
meth[c("health_worker")] = "logreg"
meth[c("opinion_h1n1_vacc_effective")] = "polr"
meth[c("opinion_h1n1_risk")] = "polr"
meth[c("opinion_h1n1_sick_from_vacc")] = "polr"
meth[c("opinion_seas_vacc_effective")] = "polr"
meth[c("opinion_seas_risk")] = "polr"
meth[c("opinion_seas_sick_from_vacc")] = "polr"
meth[c("education")] = "cart"
meth[c("income_poverty")] = "cart"
meth[c("marital_status")] = "logreg"
meth[c("rent_or_own")] = "logreg"
meth[c("employment_status")] = "polyreg"
meth[c("household_adults")] = "cart"
meth[c("household_children")] = "cart"

set.seed(777)
vDFimp = mice(vDF, method=meth, predictorMatrix=predM, m=5)
vDF <- complete(vDFimp)

sapply(vDF, function(x) sum(is.na(x)))
str(vDF)

##check NULL
lapply(vDF,function(x) any(is.null(x)))
lapply(vDF,function(x) sum(is.null(x)))

##check NAN
lapply(vDF,function(x) any(is.nan(x)))
lapply(vDF,function(x) sum(is.nan(x)))

##check INF
lapply(vDF,function(x) any(is.infinite(x)))
lapply(vDF,function(x) sum(is.infinite(x)))

##check Char
lapply(vDF,function(x) any(is.character(x)))
lapply(vDF,function(x) sum(is.character(x)))

##cleaning data
vDF <- sqldf("SELECT * FROM vDF WHERE [household_adults]>=0 AND [household_adults]<=3")
histogram(vDF$household_adults)
vDF <- sqldf("SELECT * FROM vDF WHERE [household_children]>=0 AND [household_children]<=3")
histogram(vDF$household_children)

histogram(vDF$seasonal_vaccine)

dim(vDF)
write.csv(vDF,"C:/Users/patrick.j.a.tibayan/Desktop/vDF.csv",row.names = FALSE)

vDF <- read.csv("C:/Users/patrick.j.a.tibayan/Desktop/vDF.csv") ##26707 33
str(vDF)
vDF[,grep("household",colnames(vDF),invert = TRUE)] <- lapply(vDF[,grep("household",colnames(vDF),invert = TRUE)],as.factor)
str(vDF)
##### FEATURE SELECTION: stepAIC

stepSplit <- createDataPartition(y = vDF$seasonal_vaccine, p= 0.7, list = FALSE)
stepTrain <- vDF[stepSplit,]
stepTest <- vDF[-stepSplit,]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

vLogistic <- glm(seasonal_vaccine ~ ., family = "binomial", data=stepTrain)

predLog <- predict(vLogistic, newdata = stepTest, type = "response")
predLog

predLog <- ifelse(predLog > 0.5, 1, 0)
predLog

caret::confusionMatrix(table(predLog, stepTest$seasonal_vaccine))

summary(vLogistic)

stepAIC(vLogistic, details = TRUE)

vLogisticSelect <- glm(seasonal_vaccine ~ h1n1_knowledge + behavioral_touch_face + 
                         doctor_recc_h1n1 + doctor_recc_seasonal + chronic_med_condition +
                         health_worker + opinion_h1n1_risk + 
                         opinion_h1n1_sick_from_vacc + opinion_seas_vacc_effective + 
                         opinion_seas_risk + opinion_seas_sick_from_vacc + age_group + 
                         education + race + income_poverty + rent_or_own + employment_status,  
                         family = "binomial", 
                       data = stepTrain)
summary(vLogisticSelect)

odds_ratio <- exp(vLogisticSelect$coefficients)
odds_ratio

car::vif(vLogisticSelect)

plot(vLogisticSelect, which = 4, id.n = 3)

logstep <- stepTrain
logstep$pred <- predict(vLogisticSelect, newdata = logstep, type = "response")
logstep$logofodds <- log(logstep$pred / (1 - log(logstep$pred)))

plot(logstep$household_adults, logstep$logofodds)
plot(logstep$household_children, logstep$logofodds)

predLogSelect <- predict(vLogisticSelect, newdata = stepTest, type = "response")
predLogSelect

predLogSelect <- ifelse(predLogSelect > 0.5, 1, 0)
predLogSelect

caret::confusionMatrix(table(predLogSelect, stepTest$seasonal_vaccine))

## KNN
knnstepModel <- train(seasonal_vaccine ~ h1n1_knowledge + behavioral_touch_face + 
                    doctor_recc_h1n1 + doctor_recc_seasonal + chronic_med_condition + 
                    child_under_6_months + health_worker + opinion_h1n1_risk + 
                    opinion_h1n1_sick_from_vacc + opinion_seas_vacc_effective + 
                    opinion_seas_risk + opinion_seas_sick_from_vacc + age_group + 
                    education + race + income_poverty + rent_or_own + employment_status + 
                    hhs_geo_region + census_msa + household_children, data = stepTrain, method = "knn",
                  trControl=trctrl,
                  preProcess = c("center", "scale"))
knnstepModel

predStepKnn <- predict(knnstepModel, newdata = stepTest)
predStepKnn


caret::confusionMatrix(table (predStepKnn, stepTest$seasonal_vaccine))

## SVM
svmstepModel <- train(seasonal_vaccine ~ h1n1_knowledge + behavioral_touch_face + 
                        doctor_recc_h1n1 + doctor_recc_seasonal + chronic_med_condition + 
                        child_under_6_months + health_worker + opinion_h1n1_risk + 
                        opinion_h1n1_sick_from_vacc + opinion_seas_vacc_effective + 
                        opinion_seas_risk + opinion_seas_sick_from_vacc + age_group + 
                        education + race + income_poverty + rent_or_own + employment_status + 
                        hhs_geo_region + census_msa + household_children, data = stepTrain, method = "svmLinear",
                  trControl=trctrl,
                  preProcess = c("center", "scale"))
svmstepModel

predStepSVM <- predict(svmstepModel, newdata = stepTest)
predStepSVM


caret::confusionMatrix(table (predStepSVM, stepTest$seasonal_vaccine))

## RF
rfstepModel <- train(seasonal_vaccine ~ h1n1_knowledge + behavioral_touch_face + 
                       doctor_recc_h1n1 + doctor_recc_seasonal + chronic_med_condition + 
                       child_under_6_months + health_worker + opinion_h1n1_risk + 
                       opinion_h1n1_sick_from_vacc + opinion_seas_vacc_effective + 
                       opinion_seas_risk + opinion_seas_sick_from_vacc + age_group + 
                       education + race + income_poverty + rent_or_own + employment_status + 
                       hhs_geo_region + census_msa + household_children, data = stepTrain, method = 'rf',
                 trControl = trctrl,
                 preProcess = c("center", "scale"))
rfstepModel

predStepRF <- predict(rfstepModel, newdata = stepTest)
predStepRF

caret::confusionMatrix(table(predStepRF, stepTest$seasonal_vaccine))

##### FEATURE SELECTION: CHI SQUARE

chisq.test(table(vDF$h1n1_knowledge,vDF$seasonal_vaccine))
chisq.test(table(vDF$behavioral_touch_face,vDF$seasonal_vaccine))
chisq.test(table(vDF$doctor_recc_h1n1,vDF$seasonal_vaccine))
chisq.test(table(vDF$doctor_recc_seasonal,vDF$seasonal_vaccine))
chisq.test(table(vDF$chronic_med_condition,vDF$seasonal_vaccine))
chisq.test(table(vDF$child_under_6_months,vDF$seasonal_vaccine))
chisq.test(table(vDF$health_worker,vDF$seasonal_vaccine))
chisq.test(table(vDF$opinion_h1n1_risk,vDF$seasonal_vaccine))
chisq.test(table(vDF$opinion_h1n1_sick_from_vacc,vDF$seasonal_vaccine))
chisq.test(table(vDF$opinion_seas_vacc_effective,vDF$seasonal_vaccine))
chisq.test(table(vDF$opinion_seas_risk,vDF$seasonal_vaccine))
chisq.test(table(vDF$opinion_seas_sick_from_vacc,vDF$seasonal_vaccine))
chisq.test(table(vDF$age_group,vDF$seasonal_vaccine))
chisq.test(table(vDF$education,vDF$seasonal_vaccine))
chisq.test(table(vDF$race,vDF$seasonal_vaccine))
chisq.test(table(vDF$income_poverty,vDF$seasonal_vaccine))
chisq.test(table(vDF$rent_or_own,vDF$seasonal_vaccine))
chisq.test(table(vDF$employment_status,vDF$seasonal_vaccine))
chisq.test(table(vDF$hhs_geo_region,vDF$seasonal_vaccine))
chisq.test(table(vDF$census_msa,vDF$seasonal_vaccine))
chisq.test(table(vDF$household_children,vDF$seasonal_vaccine))

chisq.test(table(vDF$h1n1_concern,vDF$seasonal_vaccine))
#chisq.test(table(vDF$behavioral_antiviral_meds,vDF$seasonal_vaccine))
chisq.test(table(vDF$behavioral_avoidance,vDF$seasonal_vaccine))
chisq.test(table(vDF$behavioral_face_mask,vDF$seasonal_vaccine))
chisq.test(table(vDF$behavioral_wash_hands,vDF$seasonal_vaccine))
chisq.test(table(vDF$behavioral_large_gatherings,vDF$seasonal_vaccine))
chisq.test(table(vDF$behavioral_outside_home,vDF$seasonal_vaccine))
chisq.test(table(vDF$opinion_h1n1_vacc_effective,vDF$seasonal_vaccine))
chisq.test(table(vDF$sex,vDF$seasonal_vaccine))
chisq.test(table(vDF$marital_status,vDF$seasonal_vaccine))
chisq.test(table(vDF$household_adults,vDF$seasonal_vaccine))

##### FEATURE SELECTION: SIGNIFICANCE 

summary(vLogistic)

sigSplit <- createDataPartition(y = vDF$seasonal_vaccine, p= 0.7, list = FALSE)
sigTrain <- vDF[sigSplit,]
sigTest <- vDF[-sigSplit,]

trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 2, verboseIter = TRUE)


sigLogModel <- glm(seasonal_vaccine ~ h1n1_knowledge + behavioral_touch_face + 
                         doctor_recc_h1n1 + doctor_recc_seasonal + chronic_med_condition + 
                         child_under_6_months + health_worker + 
                         opinion_seas_vacc_effective + 
                         opinion_seas_risk + opinion_seas_sick_from_vacc + age_group + 
                         education + race + income_poverty + marital_status + rent_or_own + 
                         employment_status + 
                         household_children, family = "binomial", 
                        data = sigTrain)
summary(sigLogModel)

sigLogPred <- predict(sigLogModel, newdata = sigTest, type="response")
sigLogPred

sigLogPred <- ifelse(sigLogPred > 0.5, 1, 0)
sigLogPred

caret::confusionMatrix(table(sigLogPred, sigTest$seasonal_vaccine))


## KNN
knnsigModel <- train(seasonal_vaccine ~ h1n1_knowledge + behavioral_touch_face + 
                       doctor_recc_h1n1 + doctor_recc_seasonal + chronic_med_condition + 
                       child_under_6_months + health_worker + 
                       opinion_seas_vacc_effective + 
                       opinion_seas_risk + opinion_seas_sick_from_vacc + age_group + 
                       education + race + income_poverty + marital_status + rent_or_own + employment_status + 
                       household_children, data = sigTrain, method = "knn",
                      trControl=trctrl,
                      preProcess = c("center", "scale"))
knnsigModel

predSigKnn <- predict(knnsigModel, newdata = sigTest)
predSigKnn


caret::confusionMatrix(table (predSigKnn, sigTest$seasonal_vaccine))

## SVM
svmsigModel <- train(seasonal_vaccine ~ h1n1_knowledge + behavioral_touch_face + 
                        doctor_recc_h1n1 + doctor_recc_seasonal + chronic_med_condition + 
                        child_under_6_months + health_worker + 
                        opinion_seas_vacc_effective + 
                        opinion_seas_risk + opinion_seas_sick_from_vacc + age_group + 
                        education + race + income_poverty + marital_status + rent_or_own + employment_status + 
                        household_children, data = sigTrain, method = "svmLinear",
                      trControl=trctrl,
                      preProcess = c("center", "scale"))
svmsigModel

predSigSVM <- predict(svmsigModel, newdata = sigTest)
predSigSVM

caret::confusionMatrix(table (predSigSVM, sigTest$seasonal_vaccine))

## RF
rfsigModel <- train(seasonal_vaccine ~ h1n1_knowledge + behavioral_touch_face + 
                      doctor_recc_h1n1 + doctor_recc_seasonal + chronic_med_condition + 
                      child_under_6_months + health_worker + 
                      opinion_seas_vacc_effective + 
                      opinion_seas_risk + opinion_seas_sick_from_vacc + age_group + 
                      education + race + income_poverty + marital_status + rent_or_own + employment_status + 
                      household_children, data = sigTrain, method = 'rf',
                     trControl = trctrl,
                     preProcess = c("center", "scale"))
rfsigModel

predSigRF <- predict(rfsigModel, newdata = sigTest)
predSigRF

caret::confusionMatrix(table(predSigRF, sigTest$seasonal_vaccine))



summary(vLogisticSelect)

sigSelectModel <- glm(seasonal_vaccine ~ h1n1_knowledge + behavioral_touch_face + 
                        doctor_recc_h1n1 + doctor_recc_seasonal + chronic_med_condition + 
                        health_worker + opinion_h1n1_risk + 
                        opinion_h1n1_sick_from_vacc + opinion_seas_vacc_effective + 
                        opinion_seas_risk + opinion_seas_sick_from_vacc + age_group + 
                        education + race + income_poverty + rent_or_own + 
                        employment_status + census_msa, family = "binomial", 
                   data = sigTrain)

sigSelPred <- predict(sigSelectModel, newdata = sigTest, type="response")
sigSelPred

sigSelPred <- ifelse(sigSelPred > 0.5, 1, 0)
sigSelPred

caret::confusionMatrix(table(sigSelPred, sigTest$seasonal_vaccine))


## KNN
knnsigSelModel <- train(seasonal_vaccine ~ h1n1_knowledge + behavioral_touch_face + 
                          doctor_recc_h1n1 + doctor_recc_seasonal + chronic_med_condition + 
                          health_worker + opinion_h1n1_risk + 
                          opinion_h1n1_sick_from_vacc + opinion_seas_vacc_effective + 
                          opinion_seas_risk + opinion_seas_sick_from_vacc + age_group + 
                          education + race + income_poverty + rent_or_own + employment_status + 
                          census_msa, data = sigTrain, method = "knn",
                     trControl=trctrl,
                     preProcess = c("center", "scale"))
knnsigSelModel

predSigSelKnn <- predict(knnsigSelModel, newdata = sigTest)
predSigSelKnn


caret::confusionMatrix(table (predSigSelKnn, sigTest$seasonal_vaccine))

## SVM
svmsigSelModel <- train(seasonal_vaccine ~ h1n1_knowledge + behavioral_touch_face + 
                          doctor_recc_h1n1 + doctor_recc_seasonal + chronic_med_condition + 
                          health_worker + opinion_h1n1_risk + 
                          opinion_h1n1_sick_from_vacc + opinion_seas_vacc_effective + 
                          opinion_seas_risk + opinion_seas_sick_from_vacc + age_group + 
                          education + race + income_poverty + rent_or_own + employment_status + 
                          census_msa, data = sigTrain, method = "svmLinear",
                     trControl=trctrl,
                     preProcess = c("center", "scale"))
svmsigSelModel

predSigSelSVM <- predict(svmsigSelModel, newdata = sigTest)
predSigSelSVM

caret::confusionMatrix(table (predSigSelSVM, sigTest$seasonal_vaccine))

## RF
rfsigSelModel <- train(seasonal_vaccine ~ h1n1_knowledge + behavioral_touch_face + 
                         doctor_recc_h1n1 + doctor_recc_seasonal + chronic_med_condition + 
                         health_worker + opinion_h1n1_risk + 
                         opinion_h1n1_sick_from_vacc + opinion_seas_vacc_effective + 
                         opinion_seas_risk + opinion_seas_sick_from_vacc + age_group + 
                         education + race + income_poverty + rent_or_own + employment_status + 
                         census_msa, data = sigTrain, method = 'rf',
                    trControl = trctrl,
                    preProcess = c("center", "scale"))
rfsigSelModel

predSigSelRF <- predict(rfsigSelModel, newdata = sigTest)
predSigSelRF

caret::confusionMatrix(table(predSigSelRF, sigTest$seasonal_vaccine))


####### FINE TUNING FINAL MODEL (SVM)

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svmTunedMod <- train(seasonal_vaccine ~ h1n1_knowledge + behavioral_touch_face + 
                       doctor_recc_h1n1 + doctor_recc_seasonal + chronic_med_condition + 
                       health_worker + opinion_h1n1_risk + 
                       opinion_h1n1_sick_from_vacc + opinion_seas_vacc_effective + 
                       opinion_seas_risk + opinion_seas_sick_from_vacc + age_group + 
                       education + race + income_poverty + rent_or_own + employment_status + 
                       census_msa, data = sigTrain, method = "svmLinear",
                     trControl=trctrl,
                     preProcess = c("center", "scale"),tuneGrid = grid,
                     tuneLength = 10)
svmTunedMod

predTunedSVM <- predict(svmTunedMod, newdata = sigTest)
predTunedSVM

caret::confusionMatrix(table (predTunedSVM, sigTest$seasonal_vaccine))

####### AUTO ML
#install.packages("h2o")
library(h2o)

h2o.init()

conv_data <- h2o.importFile("C:/Users/patrick.j.a.tibayan/Desktop/vDF.csv")
str(conv_data)

conv_data[,grep("household",colnames(conv_data),invert = TRUE)] <- as.factor(conv_data[,grep("household",colnames(conv_data),invert = TRUE)])
str(conv_data)

splith2o <- h2o.splitFrame(conv_data, c(0.6,0.2),seed = 777)

trainh2o <- h2o.assign(splith2o[[1]],"train")
validh2o <- h2o.assign(splith2o[[2]],"valid")
testh2o <- h2o.assign(splith2o[[3]],"test")

target <- "seasonal_vaccine"
predictors <- setdiff(names(trainh2o),target)

autoh2oMod <- h2o.automl(x=predictors, y=target, training_frame = trainh2o, leaderboard_frame = validh2o)

autoh2oLead <- autoh2oMod@leader
autoh2oLead

predh2oMod <- h2o.predict(object = autoh2oLead, newdata = testh2o)
predh2oMod

h2o.table(predh2oMod$predict,testh2o$seasonal_vaccine)


## with selection

vDF <- read.csv("C:/Users/patrick.j.a.tibayan/Desktop/vDF.csv")
seasonal_vaccine ~ h1n1_knowledge + behavioral_touch_face + 
  doctor_recc_h1n1 + doctor_recc_seasonal + chronic_med_condition + 
  health_worker + opinion_h1n1_risk + 
  opinion_h1n1_sick_from_vacc + opinion_seas_vacc_effective + 
  opinion_seas_risk + opinion_seas_sick_from_vacc + age_group + 
  education + race + income_poverty + rent_or_own + employment_status + 
  census_msa
vDFsub <- vDF[c('h1n1_knowledge','behavioral_touch_face','doctor_recc_h1n1','doctor_recc_seasonal','chronic_med_condition','health_worker','opinion_h1n1_risk',
      'opinion_h1n1_sick_from_vacc','opinion_seas_vacc_effective','opinion_seas_risk','opinion_seas_sick_from_vacc','age_group','education','race',
      'income_poverty','rent_or_own','employment_status','census_msa','seasonal_vaccine')]
write.csv(vDFsub,"C:/Users/patrick.j.a.tibayan/Desktop/vDFsub.csv",row.names = FALSE)

conv_dataSub <- h2o.importFile("C:/Users/patrick.j.a.tibayan/Desktop/vDFsub.csv")
str(conv_dataSub)

conv_dataSub <- as.factor(conv_dataSub)
str(conv_dataSub)

splith2oSub <- h2o.splitFrame(conv_dataSub, c(0.6,0.2),seed = 777)

trainh2oSub <- h2o.assign(splith2oSub[[1]],"train")
validh2oSub <- h2o.assign(splith2oSub[[2]],"valid")
testh2oSub <- h2o.assign(splith2oSub[[3]],"test")

target <- "seasonal_vaccine"
predictors <- setdiff(names(trainh2oSub),target)

autoh2oSubMod <- h2o.automl(x=predictors, y=target, training_frame = trainh2oSub, leaderboard_frame = validh2oSub)

autoh2oLeadSub <- autoh2oSubMod@leader
autoh2oLeadSub

predh2oModSub <- h2o.predict(object = autoh2oLeadSub, newdata = testh2oSub)
predh2oModSub

h2o.table(predh2oModSub$predict,testh2oSub$seasonal_vaccine)



####################################################################################################################################
# WITHOUT IMPUTATION

setwd("C:/Users/patrick.j.a.tibayan/Downloads/Trainings/Predictive Analytics Bootcamp/Capstone/Vaccine")

vlabel <- read.csv("Vaccine_labels.csv", stringsAsFactors = FALSE)
vfeatures <- read.csv("Vaccine_features.csv", stringsAsFactors = FALSE)

vlabel <- vlabel[c("respondent_id","seasonal_vaccine")]
vDF <- merge(vfeatures,vlabel,by="respondent_id")
vDF <- vDF[,-1]

vDF$education[which(vDF$education=="")] <- NA
vDF$income_poverty[which(vDF$income_poverty=="")] <- NA
vDF$marital_status[which(vDF$marital_status=="")] <- NA
vDF$rent_or_own[which(vDF$rent_or_own=="")] <- NA
vDF$employment_status[which(vDF$employment_status=="")] <- NA
vDF$employment_industry[which(vDF$employment_industry=="")] <- NA
vDF$employment_occupation[which(vDF$employment_occupation=="")] <- NA
vDF[,grep("household",colnames(vDF),invert = TRUE)] <- lapply(vDF[,grep("household",colnames(vDF),invert = TRUE)],as.factor)

apply(vDF,2,function(x) any(is.na(x)))
apply(vDF,2,function(x) sum(is.na(x)))

pMiss <- function(x){(sum(is.na(x))/length(x))*100}
apply(vDF,2,pMiss)

vDF <- vDF[ , -which(names(vDF) %in% c("health_insurance","employment_industry","employment_occupation"))]

vDF <- vDF[complete.cases(vDF), ]

pMiss <- function(x){(sum(is.na(x))/length(x))*100}
apply(vDF,2,pMiss)

##cleaning data
vDF <- sqldf("SELECT * FROM vDF WHERE [household_adults]>=0 AND [household_adults]<=3")
histogram(vDF$household_adults)
vDF <- sqldf("SELECT * FROM vDF WHERE [household_children]>=0 AND [household_children]<=3")
histogram(vDF$household_children)

histogram(vDF$seasonal_vaccine)
dim(vDF)
##### FEATURE SELECTION: stepAIC

stepSplit <- createDataPartition(y = vDF$seasonal_vaccine, p= 0.7, list = FALSE)
stepTrain <- vDF[stepSplit,]
stepTest <- vDF[-stepSplit,]

trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 2,verboseIter = TRUE)

vLogistic <- glm(seasonal_vaccine ~ ., family = "binomial", data=stepTrain)

predLog <- predict(vLogistic, newdata = stepTest, type = "response")
predLog

predLog <- ifelse(predLog > 0.5, 1, 0)
predLog

caret::confusionMatrix(table(predLog, stepTest$seasonal_vaccine))

summary(vLogistic)

stepAIC(vLogistic, details = TRUE)

vLogisticSelect <- glm(seasonal_vaccine ~ h1n1_knowledge + behavioral_face_mask + 
                         behavioral_outside_home + behavioral_touch_face + doctor_recc_h1n1 + 
                         doctor_recc_seasonal + chronic_med_condition + child_under_6_months + 
                         health_worker + opinion_h1n1_risk + opinion_h1n1_sick_from_vacc + 
                         opinion_seas_vacc_effective + opinion_seas_risk + 
                         opinion_seas_sick_from_vacc + age_group + education + race + 
                         income_poverty + marital_status + 
                         rent_or_own + employment_status + hhs_geo_region, family = "binomial", 
                       data = stepTrain)

car::vif(vLogisticSelect)

plot(vLogisticSelect, which = 4, id.n = 3)

logstep <- stepTrain
logstep$pred <- predict(vLogisticSelect, newdata = logstep, type = "response")
logstep$logofodds <- log(logstep$pred / (1 - log(logstep$pred)))

plot(logstep$household_adults, logstep$logofodds)
plot(logstep$household_children, logstep$logofodds)

predLogSelect <- predict(vLogisticSelect, newdata = stepTest, type = "response")
predLogSelect

predLogSelect <- ifelse(predLogSelect > 0.5, 1, 0)
predLogSelect

caret::confusionMatrix(table(predLogSelect, stepTest$seasonal_vaccine))

## SVM
svmstepModel <- train(seasonal_vaccine ~ h1n1_knowledge + behavioral_face_mask + 
                        behavioral_outside_home + behavioral_touch_face + doctor_recc_h1n1 + 
                        doctor_recc_seasonal + chronic_med_condition + child_under_6_months + 
                        health_worker + opinion_h1n1_risk + opinion_h1n1_sick_from_vacc + 
                        opinion_seas_vacc_effective + opinion_seas_risk + opinion_seas_sick_from_vacc + 
                        age_group + education + race + income_poverty + marital_status + 
                        rent_or_own + employment_status + hhs_geo_region, data = stepTrain, method = "svmLinear",
                      trControl=trctrl,
                      preProcess = c("center", "scale"))
svmstepModel

predStepSVM <- predict(svmstepModel, newdata = stepTest)
predStepSVM


caret::confusionMatrix(table (predStepSVM, stepTest$seasonal_vaccine))

## FINE TUNING SVM

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2))
svmTunedMod <- train(seasonal_vaccine ~ h1n1_knowledge + behavioral_face_mask + 
                       behavioral_outside_home + behavioral_touch_face + doctor_recc_h1n1 + 
                       doctor_recc_seasonal + chronic_med_condition + child_under_6_months + 
                       health_worker + opinion_h1n1_risk + opinion_h1n1_sick_from_vacc + 
                       opinion_seas_vacc_effective + opinion_seas_risk + opinion_seas_sick_from_vacc + 
                       age_group + education + race + income_poverty + marital_status + 
                       rent_or_own + employment_status + hhs_geo_region, data = stepTrain, method = "svmLinear",
                     trControl=trctrl,
                     preProcess = c("center", "scale"),tuneGrid = grid,
                     tuneLength = 10)
svmTunedMod

predTunedSVM <- predict(svmTunedMod, newdata = stepTest)
predTunedSVM

caret::confusionMatrix(table (predTunedSVM, stepTest$seasonal_vaccine))
