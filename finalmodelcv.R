##tuning with cross validation on individual models#


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
library(smotefamily)
library(FNN)

#load data#
setwd("C:/Users/Noah/Dropbox/BDSI Imaging Team 1/Training Data")
load("traindata_std.RData")

#load in and organize data#

datatruth <- traindata[,1]

#cross validation
set.seed(11)
svmpred = rep(0,700)
rfpred = rep(0,700)
nnetpred = rep(0,700)
knnpred = rep(0,700)
gampred = rep(0,700)
C50pred = rep(0,700)
ss = sample(700,replace=F)
for (i in seq(1,700,by=70)) {
  train=SMOTE(traindata[-ss[i:(i+69)],-1],traindata[-ss[i:(i+69)],1],K=5,dup_size = 0)
  truth=as.numeric(train$data[,57])
  train=prcomp(~ ., data = train$data[,-57], scale = TRUE)
  test=as.matrix(traindata[ss[i:(i+69)],-c(1:3)]) %*% train$rotation
  #svm#
  fin.svm = svm(factor(truth) ~ .,train$x[,1:25],kernel="radial",cost=10,gamma=0.001,probability=TRUE,type="C-classification")
  pred.svm=predict(fin.svm,newdata=as.data.frame(test[,1:25]),probability=TRUE)
  svmpred[ss[i:(i+69)]]=attr(pred.svm,"probabilities")[,1]
  #randomforest#
  fin.rf <- randomForest(truth ~ .,data=train$x[,1:25],type=classification,importance=TRUE,proximity=TRUE)
  rfpred[ss[i:(i+69)]]=predict(fin.rf,newdata=as.data.frame(test[,1:25]),type='prob')[,2]
  #ANN#
  moleANN = nnet(train$x[,1:25], class.ind(truth), size=15, softmax=TRUE,verbose=FALSE)
  nnetpred[ss[i:(i+69)]] <- predict(moleANN, as.data.frame(test[,1:25]),type='class')
  #knn
  knnpred[ss[i:(i+69)]]<- knn(train$x[,1:25],test[,1:25],truth,k=6)
  #c5.0
  fin.C50 = C5.0(factor(truth)~., data=as.data.frame(train$x[,1:25]))
  C50pred[ss[i:(i+69)]] <- predict(fin.C50, newdata=as.data.frame(test[,1:25]),type='prob')[,2]
}

#svm#
set.seed(21)
cvlist = list()
for(q in c(0.0001,0.001,0.01,0.1,1,10)){
cvresults = cbind(rep(0,20),rep(0,20),rep(0,20))
for(j in 1:20){
svmpred = rep(0,700)
ss = sample(700,replace=F)
for (i in seq(1,700,by=70)) {
  train=SMOTE(traindata[-ss[i:(i+69)],-c(1:3)],traindata[-ss[i:(i+69)],3],K=5,dup_size = 0)
  truth=as.numeric(train$data[,57])
  train=prcomp(~ ., data = train$data[,-57], scale = TRUE)
  test=as.matrix(traindata[ss[i:(i+69)],-c(1:3)]) %*% train$rotation
  #svm#
  fin.svm = svm(factor(truth) ~ .,train$x[,1:25],kernel="radial",cost=j,gamma=0.1,probability=TRUE,type="C-classification")
  pred.svm=predict(fin.svm,newdata=as.data.frame(test[,1:25]),probability=TRUE)
  svmpred[ss[i:(i+69)]]=attr(pred.svm,"probabilities")[,1]
}
cvresults[j,1]=sum(datatruth == 1 & ifelse(svmpred > .5,1,0) == 1)/sum(datatruth==1)
cvresults[j,2]=sum(datatruth == 0 & ifelse(svmpred > .5,1,0) == 0)/sum(datatruth==0)
cvresults[j,3]=(cvresults[j,1] + cvresults[j,2]) / 2
}
cvlist[[paste0("element", q)]] <- cvresults
}

#knn cv#
set.seed(21)
cvlist = list()
cvresults = cbind(rep(0,20),rep(0,20),rep(0,20))
for(j in 1:20){
    knnpred = rep(0,700)
    ss = sample(700,replace=F)
    for (i in seq(1,700,by=70)) {
      train=SMOTE(traindata[-ss[i:(i+69)],-c(1:3)],traindata[-ss[i:(i+69)],3],K=5,dup_size = 0)
      truth=as.numeric(train$data[,57])
      train=prcomp(~ ., data = train$data[,-57], scale = TRUE)
      test=as.matrix(traindata[ss[i:(i+69)],-c(1:3)]) %*% train$rotation
      #knn#
      knnpred[ss[i:(i+69)]]<- knn(train$x[,1:25],test[,1:25],truth,k=1)
    }
    cvresults[j,1]=sum(datatruth == 1 & knnpred == 2)/sum(datatruth==1)
    cvresults[j,2]=sum(datatruth == 0 & knnpred == 1)/sum(datatruth==0)
    cvresults[j,3]=(cvresults[j,1] + cvresults[j,2]) / 2
}

#nnet cv#
set.seed(22)
  nnetpredprob = rep(0,700)
  ss = sample(700,replace=F)
  for (i in seq(1,700,by=70)) {
    train=SMOTE(traindata[-ss[i:(i+69)],-c(1:3)],traindata[-ss[i:(i+69)],3],K=5,dup_size = 0)
    truth=as.numeric(train$data[,57])
    train=prcomp(~ ., data = train$data[,-57], scale = TRUE)
    test=as.matrix(traindata[ss[i:(i+69)],-c(1:3)]) %*% train$rotation
    #nnet#
    moleANN = nnet(train$x[,1:25], class.ind(truth), size=1, softmax=TRUE,rang=1/15)
    nnetpredprob[ss[i:(i+69)]] <- predict(moleANN, as.data.frame(test[,1:25]),type='raw')[,2]
  }
  cvresults = matrix(0,101,3)
  for(j in 0:100){
  cvresults[j,1]=sum(datatruth == 1 & ifelse(nnetpredprob > j/100,1,0)==1)/sum(datatruth==1)
  cvresults[j,2]=sum(datatruth == 0 & ifelse(nnetpredprob > j/100,1,0)==0)/sum(datatruth==0)
  cvresults[j,3]=(cvresults[j,1] + cvresults[j,2]) / 2
  }
plot(cvresults[,1],1-cvresults[,2],type='l')
cvresults
?nnet

hist(nnetpredprob)
nnetpredprob
