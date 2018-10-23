
# read in data
imgf = read.table("img_features2a.txt",header=F)

# extract truth into a vector
truth = imgf[,ncol(imgf)]

# remove truth from data
imgf = imgf[,-ncol(imgf)]

# run logistic regression and look at summary
glm.tmp = glm(truth~.,data=imgf,family=binomial())
summary(glm.tmp)

# remove vector that gives NA
imgf = imgf[,-49]

# run logistic regression and look at summary
glm.tmp = glm(truth~.,data=imgf,family=binomial())
summary(glm.tmp)


# now do 10 fold cross validation and predict benign/malignant status
pred1 = rep(0,700)
AUC1 = rep(0,100) 
for (j in 1:100) {
ss = sample(700,replace=F)
for (i in seq(1,700,by=70)) {
    glm.tmp = glm(truth[-ss[i:(i+69)]] ~ .,data=imgf[-ss[i:(i+69)],],family=binomial())
    pred1[ss[i:(i+69)]] = predict(glm.tmp,newdata=imgf[ss[i:(i+69)],],type="response")
}

# cross classify truth and prediction
#xtabs(~truth + ifelse(pred1<0.5,0,1))


# create specificity vs. sensitivity table at different probability thresholds
spec.sens = matrix(0,101,2)
for (i in 0:100) {
	spec.sens[i+1,1] = sum(truth == 0 & !ifelse(pred1< i/100,0,1))/sum(truth==0)
	spec.sens[i+1,2] = sum(truth == 1 & ifelse(pred1< i/100,0,1))/sum(truth==1)	
}

# which gives maximum average correct classification
#spec.sens[which.max(apply(spec.sens,1,mean)),]

# plot ROC curve
#plot(1-spec.sens[,1],spec.sens[,2],ylab="Sensitivity",xlab="1-Specificity",col=4,type="s");abline(0,1,col=1)

# calculate area under ROC curve
(AUC1[j] = sum(abs(diff(1-spec.sens[,1]))*spec.sens[1:100,2]))
}

# now extract PC (95% of variation explained)
pc.imgf = prcomp(imgf,center=T,scale=T)
summary(pc.imgf)
pcs = as.data.frame(pc.imgf$x[,1:16])

#redo above analysis
pred2 = rep(0,700)
AUC2= rep(0,100)
for (j in 1:100) {
ss = sample(700,replace=F)
for (i in seq(1,700,by=70)) {
    glm.tmp = glm(truth[-ss[i:(i+69)]] ~ .,data=pcs[-ss[i:(i+69)],],family=binomial())
    pred2[ss[i:(i+69)]] = predict(glm.tmp,newdata=pcs[ss[i:(i+69)],],type="response")
}

xtabs(~truth + ifelse(pred2<0.5,0,1))


spec.sens = matrix(0,101,2)
for (i in 0:100) {
	spec.sens[i+1,1] = sum(truth == 0 & !ifelse(pred2< i/100,0,1))/sum(truth==0)
	spec.sens[i+1,2] = sum(truth == 1 & ifelse(pred2< i/100,0,1))/sum(truth==1)	
}

#spec.sens[which.max(apply(spec.sens,1,mean)),]

#plot(1-spec.sens[,1],spec.sens[,2],ylab="Sensitivity",xlab="1-Specificity",col=4,type="s");abline(0,1,col=1)

(AUC2[j] = sum(abs(diff(1-spec.sens[,1]))*spec.sens[1:100,2]))
}


require(mboost)

#redo above analysis w/boosting (using full data, not PC scores)
pred3 = rep(0,700)
t2 = as.factor(truth)
AUC3 = rep(0,100)
for (j in 1:100) {
ss = sample(700,replace=F)
for (i in seq(1,700,by=70)) {
    glmboost.tmp = glmboost(t2[-ss[i:(i+69)]] ~ .,data=imgf[-ss[i:(i+69)],],family=Binomial(type="adaboost"))
    pred3[ss[i:(i+69)]] = predict(glmboost.tmp,newdata=imgf[ss[i:(i+69)],],type="response")
}

# xtabs(~truth + ifelse(pred3<0.5,0,1))


spec.sens = matrix(0,101,2)
for (i in 0:100) {
	spec.sens[i+1,1] = sum(truth == 0 & !ifelse(pred3< i/100,0,1))/sum(truth==0)
	spec.sens[i+1,2] = sum(truth == 1 & ifelse(pred3< i/100,0,1))/sum(truth==1)	
}

#spec.sens[which.max(apply(spec.sens,1,mean)),]

#plot(1-spec.sens[,1],spec.sens[,2],ylab="Sensitivity",xlab="1-Specificity",col=4,type="s");abline(0,1,col=1)

(AUC3[j] = sum(abs(diff(1-spec.sens[,1]))*spec.sens[1:100,2]))
}


summary(AUC1)   #summary AUC for glm w/imgf
summary(AUC2)  #summary AUC using PC scores
summary(AUC3)  #summary AUC using glmboost w/imgf


par(mfrow=c(3,1))
hist(AUC1,xlim=c(0.65,0.72))
hist(AUC2,xlim=c(0.65,0.72))
hist(AUC3,xlim=c(0.65,0.72))
