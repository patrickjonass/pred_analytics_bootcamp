v <- factor(c("2","3", "5", "7", "11"))
str(v)
as.character(v)
str(as.character(v))
as.numeric(v)
str(as.numeric(v))
as.integer(as.character(v))
str(v)


library("readxl")
data <- read.csv("C:\\Users\\patrick.j.a.tibayan\\Downloads\\Trainings\\Predictive Analytics Bootcamp\\First Checkpoint\\Iris_Data.csv")
data

data$Sepal.Length <- as.numeric(data$Sepal.Length)
data$Sepal.Width <- as.numeric(data$Sepal.Width)
data$Petal.Length <- as.numeric(data$Petal.Length)
data$Petal.Width <- as.numeric(data$Petal.Width)


nrow(data[rowSums(is.na(data))>0,])/nrow(data)
nrow(data[rowSums(is.na(data))==0,])
nrow(data[rowSums(is.na(data))==0,])/nrow(data)

##check NA
apply(data,2,function(x) any(is.na(x)))
apply(data,2,function(x) sum(is.na(x)))

##check NULL
lapply(data,function(x) any(is.null(x)))
lapply(data,function(x) sum(is.null(x)))

##check NAN
lapply(data,function(x) any(is.nan(x)))
lapply(data,function(x) sum(is.nan(x)))

##check INF
lapply(data,function(x) any(is.infinite(x)))
lapply(data,function(x) sum(is.infinite(x)))


data$Petal.Width[which(is.infinite(data$Petal.Width))]<- NA

##check INF
lapply(data,function(x) any(is.infinite(x)))
lapply(data,function(x) sum(is.infinite(x)))

unique(data$Species)

cleanData <- data
library(sqldf)

cleanData <- sqldf("SELECT * FROM cleanData WHERE (Species=='setosa' OR Species=='versicolor' OR Species=='virginica')")
cleanData <- sqldf("SELECT * FROM cleanData WHERE ([Sepal.Length]>0 AND [Sepal.Width]>0 AND [Petal.Length]>0 AND [Petal.Width]>0)")
cleanData <- sqldf("SELECT * FROM cleanData WHERE [Petal.Length]>=2*[Petal.Width]")
cleanData <- sqldf("SELECT * FROM cleanData WHERE [Sepal.Length]<=30")
cleanData <- sqldf("SELECT * FROM cleanData WHERE ([Sepal.Length]>[Petal.Length] AND [Sepal.Width]>[Petal.Width])")
nrow(cleanData)/nrow(data)


boxplot.stats(data$Petal.Length)
sqldf("SELECT * FROM data WHERE ([Petal.Length]==63 OR [Petal.Length]==23 OR [Petal.Length]==14)")
data[which((data$Petal.Length==63)|(data$Petal.Length==23)|(data$Petal.Length==14)),]

data$Petal.Width[which(data$Petal.Width<=0)] <- NA
data$Petal.Width[data$Petal.Width<=0]

boxplot(data$Sepal.Length)
boxplot.stats(data$Sepal.Length)



