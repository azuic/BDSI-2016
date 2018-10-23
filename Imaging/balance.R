## separate traindata.pca
melanoma <- traindata.pca[traindata.pca$Malignant==1,]
benign <- traindata.pca[traindata.pca$Malignant==0,]

melanoma.sd <- apply(melanoma[,-1],2,sd)
benign.sd <- apply(benign[,-1],2,sd)
all.sd <- data.frame(names(melanoma.sd), melanoma.sd, benign.sd,row.names = NULL)
colnames(all.sd)[1] <- "name"
all.sd.long <- melt(all.sd, id="name")
ggplot(data=all.sd.long,aes(x=factor(name,levels = unique(name)),y=value,colour=variable)) + geom_point()

melanoma.mean <- apply(melanoma[,-1],2,sd)
benign.mean <- apply(benign[,-1],2,sd)
all.mean <- data.frame(names(melanoma.mean), melanoma.mean, benign.mean,row.names = NULL)
colnames(all.mean)[1] <- "name"
all.mean.long <- melt(all.mean, id="name")
ggplot(data=all.mean.long,aes(x=factor(name,levels = unique(name)),y=value,colour=variable)) + geom_point()


## SMOTE: generating new minority using synthetic method
balance.smote <- SMOTE(X=traindata.pca[,-1],target=traindata.pca$Malignant, K=3)$data

original.sd <- apply(traindata.pca[,-1],2,sd)
smote.sd <- apply(balance.smote[,-1],2,sd)
rose.sd <- apply(data.rose[,-1],2,sd)
sd <- data.frame(names(original.sd), original.sd, smote.sd, rose.sd, row.names = NULL)
colnames(sd)[1] <- "name"
sd.long <- melt(sd, id="name")
ggplot(data=sd.long,aes(x=factor(name,levels = unique(name)),y=value,colour=variable)) + geom_point()

original.mean <- apply(traindata.pca[,-1],2,mean)
smote.mean <- apply(balance.smote[,-1],2,mean)
rose.mean <- apply(data.rose[,-1],2,mean)
mean <- data.frame(names(original.mean), original.mean, smote.mean, rose.mean, row.names = NULL)
colnames(mean)[1] <- "name"
mean.long <- melt(mean, id="name")
ggplot(data=mean.long,aes(x=factor(name,levels = unique(name)),y=value,colour=variable)) + geom_point()





## SMOTE: generating new minority using synthetic method
balance.smote.std <- balance.smote
for(i in 2:26){balance.smote.std[,i]<-scale(balance.smote.std[,i])}
smote.std.sd <- apply(balance.smote.std[,-1],2,sd)
smote.std.mean <- apply(balance.smote.std[,-1],2,mean)

sd.std <- data.frame(names(original.sd),original.sd,smote.std.sd,row.names = NULL)
colnames(sd.std)[1] <- "name"
sd.long.std <- melt(sd.std, id="name")
attributes(sd.long.std)
ggplot(data=sd.long.std,aes(x=name,y=value,colour=variable)) + geom_point()

mean.std <- data.frame(names(original.mean),original.mean,smote.std.mean,row.names = NULL)
colnames(mean.std)[1] <- "name"
mean.long.std <- melt(mean.std, id="name")
ggplot(data=mean.long.std,aes(x=name,y=value,colour=variable)) + geom_point()




## bootstrap


