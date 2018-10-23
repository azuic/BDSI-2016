###### Predictions #####
## pkgs
library(e1071)
library(class)
library(randomForest)


traindata <- balance.scaled
traintruth <- traindata$Malignant
pca<- prcomp(~ ., data = traindata[,-1], scale = TRUE)
trainpca <- data.frame(traintruth,pca$x)
pca.mtx <- pca$rotation

testdata <- as.data.frame(finaldat_1_200)
testdata <- data.matrix(testdata)
testdata <- scale(testdata) %*% pca.mtx
testdata <- as.data.frame(testdata)



########################################################
##### predictions #####
## svm
fit.svm <- svm(traintruth ~ .,data=trainpca[,1:25],kernel="radial", type="C-classification",
               cost=10,gamma=0.1,probability=TRUE)
pred.svm <- predict(fit.svm, newdata=testdata[,1:25],probability = TRUE)
pred.sv.prob <- attr(pred.svm,"probabilities")

## random forest
fit.rf <- randomForest(as.factor(traintruth) ~ .,data=trainpca[,1:25],type=classification,
                       importance=TRUE,proximity=TRUE)
pred.rf <- predict(fit.rf,newdata = testdata[,1:25])
pred.rf.prob <- predict(fit.rf,newdata = testdata[,1:25], type="prob")

## ann
#ANN#
fit.ann <- nnet(trainpca[,-1], class.ind(traintruth), size=10, softmax=TRUE)
pred.ann <- predict(fit.ann, testdata,type="class",verbose=FALSE)

preds <- data.frame(pred.svm,pred.rf,as.factor(pred.ann))

################################################################
########################### CVs ################################
################################################################
#support vector machine#
#tuning
tune.out.rad=tune(svm,datatruth~.,data=trainpca,kernel="radial",
                  ranges=list(cost=c(0.1,1,10,100,1000),
                              gamma=c(0.0001,.0005,.001,.005,.01,.1,.5,1)))
#cross validation
svmpred = rep(0,1105)
ss = sample(1105,replace=F)
for(j in c(109,109,109,109,109,109,110,110,111)){
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
for(j in c(109,109,109,109,109,109,110,110,111)){
  for (i in c(1,111,221,331,441,551,661,772,883,994)) {
    train=prcomp(~ ., data = traindata[-ss[i:(i+j)],-1], scale = TRUE)
    test=as.matrix(traindata[ss[i:(i+j)],-1]) %*% train$rotation
    truth=as.factor(traindata[-ss[i:(i+j)],1])
    fin.rf = randomForest(factor(truth) ~ .,data=train$x[,1:25],type=classification,importance=TRUE,proximity=TRUE)
    rfpred[ss[i:(i+j)]]=predict(fin.rf,newdata=as.data.frame(test))
  }
}

rfpred
sum(datatruth == 0 & rfpred==1)/sum(datatruth==0)
sum(datatruth == 1 & rfpred==2)/sum(datatruth==1)
(sum(datatruth == 0 & rfpred==1)/sum(datatruth==0) + sum(datatruth == 1 & rfpred==2)/sum(datatruth==1)) /2



fin.rf = randomForest(factor(truth)~ .,data=train,type=classification,importance=TRUE,proximity=TRUE)
rfpred=predict(fin.rf,newdata=train)



################################################################
###################### Extract testing data ####################
################################################################

###### Library #####
library(jpeg)
library(png)
library(radiomics)


##### Load Data #####
yourpath <- "/home/zuic/Imaging/testing_set"

files <- list.files(path=yourpath, pattern=".jpg", all.files=TRUE, full.names=TRUE)
images_list_1_200 <- lapply(X=files, FUN=readJPEG, native=FALSE)

seg_files <- list.files(path = yourpath, pattern = ".png", full.names = TRUE)
seg_list_1_200 <- lapply(X=seg_files, FUN=readPNG, native=FALSE)

###### Define a function to convert a single image to gray-scale image
gray_scale <- function(inputImage){
  outputImage <- inputImage[,,1]*0.21 + inputImage[,,2]*0.72 + inputImage[,,3]*0.07
}
# Define a function to remove background
lesion <- function(inputImage, inputSeg){
  is.na(inputSeg) <- inputSeg == 0
  lesionImage <- inputImage*inputSeg
}

outImage_1_200 <- lapply(images_list_1_200, FUN=gray_scale)
outlesions_1_200 <- lapply(1:200, function(i) lesion(outImage_1_200[[i]],seg_list_1_200[[i]]))

save(outlesions_1_200, file="outlesions_1_200.RData")


glcmDat_1_200 <- lapply(outlesions_1_200, FUN=glcm,n_grey=32)
glrlmDat_1_200 <- lapply(outlesions_1_200, FUN=glrlm)
glszmDat_1_200 <- lapply(outlesions_1_200, FUN=glszm)
finaldat_1_200 <- t(rbind(sapply(outlesions_1_200,FUN=calc_features),	# First order features
                           sapply(glcmDat_1_200,FUN=calc_features), # Grey Level Co-occurrence Matrix (GLCM) features
                           sapply(glrlmDat_1_200,FUN=calc_features),	# Grey Level Run Length Matrix (GLRLM) features
                           sapply(glszmDat_1_200,FUN=calc_features)))	# Grey Level Size Zone Matrix (GLSZM) features
