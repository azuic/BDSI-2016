##stacking##

#load data#
setwd("C:/Users/Noah/Dropbox/BDSI Imaging Team 1")
load("balance_scaled.RData")

#packages#
library(e1071)
library(class)

try.smote <- SMOTE(traindata2,traindata2$Malignant,K=5, dup_size = 0)$data

traindata2 <- traindata
traindata2[,c(1:2)] <- NULL
traindata2$Malignant <- as.factor(traindata2$Malignant)
datatruth = traindata2[,1]
#support vector machine#
#tuning
tune.out.rad=tune(svm,Malignant~.,data=data.rose,kernel="radial",
                  ranges=list(cost=c(0.1,1,10,100,1000),
                              gamma=c(0.0001,.0005,.001,.005,.01,.1,.5,1)))
#cross validation
svmpred = rep(0,700)
ss = sample(700,replace=F)
for (i in seq(1,700,by=70)) {
  #train <- SMOTE(traindata2[-ss[i:(i+69)],],traindata2$Malignant[-ss[i:(i+69)]],K=5,dup_size = 0)$data[,-58]
  train <- traindata2[-ss[i:(i+69)],]
  truth=as.factor(train$Malignant)
  trainpca=prcomp(~ ., data = train[,-1], scale = TRUE)
  test=as.matrix(traindata2[ss[i:(i+69)],-1]) %*% trainpca$rotation
  
  fin.svm = svm(truth ~ .,data=trainpca$x[,1:25],kernel="radial",cost=10,gamma=0.1)
  svmpred[ss[i:(i+69)]]=predict(fin.svm,newdata=as.data.frame(test))
}
plot(trainpca$x[,1],trainpca$x[,2])
points(test[,1],test[,2],col=2)
matplot(c(train$x[,1],test[,1]),c(trainpca$x[,2],test[,2]))
svmpred
sum(datatruth == 0 & svmpred==1)/sum(datatruth==0)
sum(datatruth == 1 & svmpred==2)/sum(datatruth==1)	
(sum(datatruth == 0 & svmpred==1)/sum(datatruth==0) + sum(datatruth == 1 & svmpred==2)/sum(datatruth==1)) /2	

fin.svm = svm(factor(truth) ~ .,data=train,kernel="radial",cost=10,gamma=0.1)
svmpred=predict(fin.svm,newdata=train)

#knn#
knnpred = rep(0,700)
ss = sample(700,replace=F)
for (i in seq(1,700,by=70)) {
  train=prcomp(~ ., data = traindata[-ss[i:(i+69)],-c(1:3)], scale = TRUE)
  test=as.matrix(traindata[ss[i:(i+69)],-c(1:3)]) %*% train$rotation
  truth=traindata[-ss[i:(i+69)],3]
  knnpred[ss[i:(i+69)]] = knn(train$x,
                            test,
                            factor(truth),
                            k=10)
}
knnpred 


