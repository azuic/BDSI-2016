## SMOTE on scaled train then pca: 
## pca results: pca.smote.scaled.train0
## pca scores:  smote.scaled.train0.pca


## SMOTE on pca: smote.train.pca0



## Disorder
k <- rep(0,1105)
k <- sample(1:1105)
balance.scaled <- smote.scaled.train0
for (i in 1:1105) {
  balance.scaled[k[i],] <- smote.scaled.train0[i,]
}


## random forest
knnpred = rep(0,1105)
ss = sample(1105,replace=F)
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