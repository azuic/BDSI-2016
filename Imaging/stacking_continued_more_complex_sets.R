setwd("C:/Users/andre_000/Dropbox/BDSI Imaging Team 1")
library("mboost")
library(caret)
library(caretEnsemble)
library(caretList)
library(plyr)
library(ipred)
load("train_balance_rose.RData")

load("data.disorder.RData")

# Stacking algorithms
# create submodels
levels(data.diso$Malignant)=make.names(levels(data.diso$Malignant))
levels(data.rose$Malignant)=make.names(levels(data.rose$Malignant))
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('svmRadial', 'bag', 'gamboost')
seed = 7
set.seed(seed)
models <- caretList(Malignant~., data=data.diso, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)
#find correlation between variables and graph it
modelCor(results)
splom(results)


# stack using glm
stackControl <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE)
set.seed(seed)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)

# stack using random forest
set.seed(seed)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)


