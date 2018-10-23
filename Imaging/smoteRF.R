library(ROCR)
library(caret)
library(randomForest)


data.smote <- balance.smote
data.smote$Malignant <- as.factor(data.smote$Malignant)
imgfdat <- data.smote

smote.rf <- randomForest(Malignant~.,data =data.smote,
                       type=classification,importance=TRUE,proximity=TRUE)
print(smote.rf)
plot(smote.rf)

varImpPlot(smote.rf, sort = T, main="Variable Importance",n.var=5)


rf.predictions <- as.vector(smote.rf$votes[,2])
prd <- prediction(rf.predictions, data.smote$Malignant)

perf_AUC <- performance(prd,"auc") #Calculate the AUC value
AUC <- perf_AUC@y.values[[1]]

perf_ROC <- performance(prd,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))


rf.pred.2 <- predict(smote.rf,newdata = traindata.pca, type="prob")
result.roc <- roc(traindata.pca$Malignant, rf.pred.2$melanoma)
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")


#calculate the 10-folds CV AUC 
# AUC2= rep(0,10)
# for (j in 1:10) {
#   pred2 = rep(0,1105)
#   ss = sample(1105,replace=F)
#   for (i in seq(1,1105,by=110)) {
#     img.rf=randomForest(imgfdat[-ss[i:(i+109)],1]~.,dat=imgfdat[-ss[i:(i+109)],-1]) 
#     pred2[ss[i:(i+109)]] = (predict(img.rf, newdata=imgfdat[ss[i:(i+109)],-1],type="prob"))[,2]
#   }
#   spec.sens = matrix(0,101,2)
#   for (i in 0:100) {
#     spec.sens[i+1,1] = sum(truth == 0 & !ifelse(pred2< i/100,0,1))/sum(truth==0)
#     spec.sens[i+1,2] = sum(truth == 1 & ifelse(pred2< i/100,0,1))/sum(truth==1)	
#   }
#   
#   #spec.sens[which.max(apply(spec.sens,1,mean)),]
#   
#   plot(1-spec.sens[,1],spec.sens[,2],ylab="Sensitivity",xlab="1-Specificity",col=4,type="s");abline(0,1,col=1)
#   
#   (AUC2[j] = sum(abs(diff(1-spec.sens[,1]))*spec.sens[1:100,2]))
# }
# max((spec.sens[,1]+spec.sens[,2])/2)
# mean(AUC2)
# hist(AUC2)
# 
