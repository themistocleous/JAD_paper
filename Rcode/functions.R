# This is just for fun but you never know!


# Clear Memory
rm(list = ls(all.names = TRUE))

# Import Libraries in R
library("ggplot2")
library("gridExtra")
library("ez")
library("lme4")
library("car")
library("lmerTest")
library("pracma")
library("gmodels")
library("klaR")
library("C50")
library("caret")
library("latticeExtra")
library("phonR")
library("plyr") # for renaming
library("xtable")

# Printing
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
options(digits=2)
options(scipen=999)

# Definitions
.str <- function(x){
  # Modified str() to display all list elements within a level.
  # Charalambos Themistocleous 2015
  str(x, list.len=length(x))
}

# Get the trimmed mean predefined value at the 10%
mean1 <- function(x,...){
  mean(x,trim=.1)
}

v <- read.csv("phone_processed.csv")

14376*.8
# splitting the dataset
dataset <- v[sample(nrow(v)),5:128]
train <- dataset[1:11501, ]
train <-  na.omit(train)
test  <- dataset[11502:14376, ]
test <-  na.omit(test)
.str(v)
prop.table(table(train$variant))
prop.table(table(test$variant))

# Classification
control <- trainControl(method="repeatedcv",classProbs = TRUE, number=10, repeats=3)
metric <- "ROC"
set.seed(7)
grid <- expand.grid( .winnow = c(FALSE), .trials=c(1,5,10,15,20), .model="tree" )
c50<- train(factor(variant) ~ . ,
            data=train,
            preProcess = c("center", "scale"),
            method="C5.0",
            grid = grid,
            metric=metric, 
            trControl=control)
summary(c50)
getTrainPerf(c50)
confusionMatrix(c50)
plot(c5.0)
pred <- predict(c50, newdata = test)
pred
confusionMatrix(pred, test$variant)
source('C5.0.graphviz.r')
C5.0.graphviz(c50$finalModel, 'treeout.txt')


rf1<- train( factor(variant) ~ .,
             data=train,
             preProcess = c("center", "scale"),
             method="rf", 
             metric=metric, 
             trControl=control)

rf1
summary(rf1$finalModel$forest)
getTrainPerf(rf1)
confusionMatrix(rf1)
plot(rf1)
pred <- predict(rf1, newdata = test)
pred
table(pred, test$accuracy)
confusionMatrix(pred, test$accuracy)
rf1_importance <- varImp(rf1, scale=FALSE)
rf1_importance
plot(rf1_importance)
