##stacking##

#load data#
setwd("C:/Users/Noah/Dropbox/BDSI Imaging Team 1")
load("balance_scaled.RData")

#packages#
library(e1071)
library(class)
library(randomForest)


traindata <- balance.scaled
datatruth <- traindata[,1]
trainpca<- prcomp(~ ., data = traindata[,-1], scale = TRUE)
trainpca <- as.data.frame(cbind(datatruth,trainpca$x))
#support vector machine#
#tuning
tune.out.rad=tune(svm,datatruth~.,data=trainpca,kernel="radial",
                  ranges=list(cost=c(0.1,1,10,100,1000),
                              gamma=c(0.0001,.0005,.001,.005,.01,.1,.5,1)))
#cross validation
svmpred = rep(0,1105)
ss = sample(1105,replace=F)
for(j in c(109,109,109,109,109,109,110,110,110)){
for (i in c(1,111,221,331,441,551,661,772,883,994)) {
  train=prcomp(~ ., data = traindata[-ss[i:(i+j)],-1], scale = TRUE)
  test=as.matrix(traindata[ss[i:(i+j)],-1]) %*% train$rotation
  truth=as.factor(traindata[-ss[i:(i+j)],1])
  fin.svm = svm(truth ~ .,data=train$x[,1:25],kernel="radial",cost=10,gamma=0.1)
  svmpred[ss[i:(i+j)]]=predict(fin.svm,newdata=as.data.frame(test))
}}

svmpred
sum(datatruth == 0 & svmpred==1)/sum(datatruth==0)
sum(datatruth == 1 & svmpred==2)/sum(datatruth==1)	
(sum(datatruth == 0 & svmpred==1)/sum(datatruth==0) + sum(datatruth == 1 & svmpred==2)/sum(datatruth==1)) /2	

fin.svm = svm(factor(truth) ~ .,data=train,kernel="radial",cost=10,gamma=0.1)
svmpred=predict(fin.svm,newdata=train)

#random forest#
rfpred = rep(0,1105)
ss = sample(1105,replace=F)
for(j in c(109,109,109,109,109,109,110,110,110)){
    for (i in c(1,111,221,331,441,551,661,772,883,994)) {
        train=prcomp(~ ., data = traindata[-ss[i:(i+j)],-1], scale = TRUE)
        test=as.matrix(traindata[ss[i:(i+j)],-1]) %*% train$rotation
        truth=as.factor(traindata[-ss[i:(i+j)],1])
        fin.rf = randomForest(factor(truth) ~ .,data=train$x[,1:25],type=classification,importance=TRUE,proximity=TRUE)
        rfpred[ss[i:(i+j)]]=predict(fin.rf,newdata=as.data.frame(test))
    }}

rfpred
sum(datatruth == 0 & rfpred==1)/sum(datatruth==0)
sum(datatruth == 1 & rfpred==2)/sum(datatruth==1)
(sum(datatruth == 0 & rfpred==1)/sum(datatruth==0) + sum(datatruth == 1 & rfpred==2)/sum(datatruth==1)) /2

fin.rf = randomForest(factor(truth)~ .,data=train,type=classification,importance=TRUE,proximity=TRUE)
rfpred=predict(fin.rf,newdata=train)


######################################################################################
####################################### END ##########################################
######################################################################################
#knn# DON'T USE
#cross validation
knnpred = rep(0,1105)
ss = sample(1105,replace=F)
for(j in c(109,109,109,109,109,109,110,110,110)){
for (i in c(1,111,221,331,441,551,661,772,883,994)) {
     train=prcomp(~ ., data = traindata[-ss[i:(i+j)],-1], scale = TRUE)
     test=as.matrix(traindata[ss[i:(i+j)],-1]) %*% train$rotation
     truth=as.numeric(as.character((traindata[-ss[i:(i+j)],1])))
     fin.knn = knn(train$x[,1:25],test,truth)
     knnpred[ss[i:(i+j)]]=predict(fin.knn,newdata=as.data.frame(test))
}}

knnpred
