##stacking#


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
library(DMwR)
library(FNN)
library(MASS)

#load data#
setwd("C:/Users/Noah/Dropbox/BDSI Imaging Team 1/Training Data")
load("traindata_std.RData")

#load in and organize data#

datatruth <- traindata[,3]

#cross validation
set.seed(11)
svmpred = rep(0,700)
rfpred = rep(0,700)
nnetpred1 = rep(0,700)
nnetpred15 = rep(0,700)
knnpred = rep(0,700)
gampred = rep(0,700)
C50pred = rep(0,700)
ldapred = rep(0,700)
gbmpred = rep(0,700)
ss = sample(700,replace=F)
for (i in seq(1,700,by=70)) {
  truth<-traindata[-ss[i:(i+69)],3]
  train=prcomp(~ ., data = traindata[-ss[i:(i+69)],-c(1:3)], scale = TRUE)
  test=as.matrix(traindata[ss[i:(i+69)],-c(1:3)]) %*% train$rotation
  #svm#
  fin.svm = svm(factor(truth) ~ .,train$x[,1:25],kernel="radial",cost=10,gamma=0.1,probability=TRUE,type="C-classification")
  pred.svm=predict(fin.svm,newdata=as.data.frame(test[,1:25]),probability=TRUE)
  svmpred[ss[i:(i+69)]]=attr(pred.svm,"probabilities")[,2]
  #randomforest#
  fin.rf <- randomForest(factor(truth) ~ .,data=train$x[,1:25],type='classification',importance=TRUE,proximity=TRUE)
  rfpred[ss[i:(i+69)]]=predict(fin.rf,newdata=as.data.frame(test[,1:25]),type='prob')[,2]
  #NNET Node 1#
  moleANN = nnet(train$x[,1:25], class.ind(truth), size=1, softmax=TRUE,verbose=FALSE)
  nnetpred1[ss[i:(i+69)]] <- predict(moleANN, as.data.frame(test[,1:25]),type='raw',rang=1/15)[,2]
  #NNET Node 15#
  moleANN = nnet(train$x[,1:25], class.ind(truth), size=15, softmax=TRUE,verbose=FALSE)
  nnetpred15[ss[i:(i+69)]] <- predict(moleANN, as.data.frame(test[,1:25]),type='raw',rang=1/15)[,2]
  #knn
  knnpred[ss[i:(i+69)]]<- knn(train$x[,1:25],test[,1:25],truth,k=1)
  #c5.0
  fin.C50 = C5.0(factor(truth)~., data=as.data.frame(train$x[,1:25]))
  C50pred[ss[i:(i+69)]] <- predict(fin.C50, newdata=as.data.frame(test[,1:25]),type='prob')[,2]
  #lda
   lda.mod <- lda(factor(truth) ~ .,data=data.frame(train$x[,1:25]),prior=c(.8,.2))
   ldapred[ss[i:(i+69)]]=predict(lda.mod,newdata=as.data.frame(test[,1:25]),prior=c(.8,.2))$posterior[,2]
   # #gbm
   gbm.mod <- gbm(truth ~ .,data=data.frame(train$x[,1:25]),n.trees = 5000,interaction.depth=1,distribution = "bernoulli")
   gbmpred[ss[i:(i+69)]] <- predict(gbm.mod,newdata=as.data.frame(test[,1:25]),type="response",n.trees=5000)
}

#formate metadata and turn predictions into binaries at optimized thresholds#
metadata <- data.frame(cbind(datatruth,svmpred,rfpred,nnetpred1,nnetpred15,knnpred,C50pred,ldapred,gbmpred))
names(metadata) <- c("truth","svmpred","rfpred","nnetpred1","nnetpred15","knnpred",'c50pred','ldapred','gbmpred')

#svm threshold = .1
#nnetpred1 threshold = .6
#nnetpred15 threshold =.1
#c5o threshold =.05
#rf threshold = .16
#lda threshold = .14
#gbm threshold = .18
hist(svmpred)
hist(metadata$svmpred)
metadata$knnbin <- as.numeric(metadata$knnpred) - 1
metadata$svmbin <- ifelse(metadata$svmpred > .4,1,0)
metadata$rfbin <- ifelse(metadata$rfpred > .16,1,0)
metadata$nnet1bin <- ifelse(metadata$nnetpred1 > .2,1,0)
metadata$nnet15bin <- ifelse(metadata$nnetpred15 > .1,1,0)
metadata$c50bin <- ifelse(metadata$c50pred > .05,1,0)
metadata$ldabin <- ifelse(metadata$ldapred > .2,1,0)
metadata$gbmbin <- ifelse(metadata$gbmpred > .3,1,0)


write.csv(metadata,"metadatatrain.csv")
voting <- with(metadata, ifelse((svmbin + rfbin + nnet1bin + nnet15bin +knnbin + c50bin + gbmbin + ldabin)/8 >= .5,1,0))


#metalearner#

metaboost = rep(0,700)
ss = sample(700,replace=F)
for (i in seq(1,700,by=70)) {
  train=metadata[-ss[i:(i+69)],]
  test=metadata[ss[i:(i+69)],]
  metaboostpred=gbm(truth ~ knnbin + svmbin + rfbin + nnet1bin + nnet15bin + c50bin + ldabin +gbmbin ,data=train, 
                    n.trees = 4000,distribution="bernoulli",interaction.depth = 4)
  metaboost[ss[i:(i+69)]]=predict(metaboostpred,newdata=as.data.frame(test),type="response",n.trees=4000)
}
cvresults = cbind(rep(0,101),rep(0,101),rep(0,101))
for(j in 1:100){
  cvresults[j,1]=sum(datatruth == 1 & ifelse(metaboost > j/100,1,0) == 1)/sum(datatruth==1)
  cvresults[j,2]=sum(datatruth == 0 & ifelse(metaboost > j/100,1,0) == 0)/sum(datatruth==0)
  cvresults[j,3]=(cvresults[j,1] + cvresults[j,2]) / 2
}
max(cvresults[,3])
cvresults
pred <- ifelse(metaboost > .22,1,0)
hist(pred)
table(metadata$c50bin,datatruth)
hist(voting)
hist(metadata$c50pred)
