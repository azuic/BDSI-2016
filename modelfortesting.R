##stacking##

#packages#
library(e1071)
library(class)
library(randomForest)
library(nnet)
library(kernlab)
library(caret)
library(class)
library(gbm)
library(C50)


#load in and organize data#
setwd("C:/Users/Noah/Dropbox/BDSI Imaging Team 1/Training Data")
load("balance_scaled.RData")


truth <- balance.scaled[,1]
train <- prcomp(~ ., data = balance.scaled[,-1], scale = TRUE)

#test data#
load('traindata_std.RData')
testdata <- traindata
test <- as.matrix(testdata[,-c(1:3)]) %*% train$rotation
testtruth <- testdata[,3]

#fit models to data#
#svm#
fin.svm <- svm(truth ~ .,data=train$x[,1:25],kernel="radial",cost=10,gamma=0.1)
svmpred <- predict(fin.svm,newdata=as.data.frame(test[,1:25]),type='response')
#randomforest#
fin.rf <- randomForest(truth ~ .,data=train$x[,1:25],type=classification,importance=TRUE,proximity=TRUE)
rfpred <- predict(fin.rf,newdata=as.data.frame(test[,1:25]),type='response')
#ANN#
moleANN <- nnet(balance.scaled[,-1], class.ind(truth), size=1, softmax=TRUE,lineout=T)
nnetpred <- predict(moleANN, as.data.frame(test[,-1]),type="class")
#knn
knnpred <- knn(train$x[,1:25],test[,1:25],truth,k=1)
#c5.0
fin.C50 <- C5.0(truth~., data=as.data.frame(train$x[,1:25]))
C50pred <- predict(fin.C50, newdata=as.data.frame(test[,1:25]))


#Put all predictions into a dataframe and format the data#
metadata <- data.frame(cbind(testtruth,svmpred,rfpred,nnetpred,knnpred,C50pred))
names(metadata) <- c("truth","svmpred","rfpred","annpred","knnpred",'c50pred')
metadata$svmpred <- as.numeric(metadata$svmpred) - 1
metadata$rfpred <- as.numeric(metadata$rfpred) - 1
metadata$truth <- as.numeric(metadata$truth) - 1
metadata$annpred <- as.numeric(metadata$annpred) - 1
metadata$knnpred <- as.numeric(metadata$knnpred) - 1
metadata$c50pred <- as.numeric(metadata$c50pred) - 1

#load in training metadata#
trainmeta <- read.csv("metadatatrain.csv")

#meta learner#
mforest <- randomForest(factor(truth) ~ svmpred + rfpred + annpred + knnpred + c50pred ,data=trainmeta,
                     mtry=3, importance=TRUE)
finalpredict <- as.numeric(predict(mforest,metadata,type='response')) -1

table(finalpredict,testtruth)
