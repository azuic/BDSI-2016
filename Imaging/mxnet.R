train.x <- as.matrix(traindata.pca[,-1])
train.y <- as.array(traindata.pca[,1])
mx.set.seed(0)
fit.mxnet.unbalance <- mx.mlp(train.x, train.y, hidden_node=10, out_node=2, out_activation="softmax",
                num.round=20, array.batch.size=15, learning.rate=0.07, momentum=0.9, 
                eval.metric=mx.metric.accuracy)

train.x.rose <- as.matrix(data.rose[,-1])
train.y.rose <- as.array(data.rose[,1])
mx.set.seed(0)
fit.mxnet.rose <- mx.mlp(train.x.rose, train.y.rose, hidden_node=10, out_node=2, out_activation="softmax",
                              num.round=20, array.batch.size=15, learning.rate=0.07, momentum=0.9, 
                              eval.metric=mx.metric.accuracy)

f_K_fold <- function(Nobs,K=5){
  rs <- runif(Nobs)
  id <- seq(Nobs)[order(rs)]
  k <- as.integer(Nobs*seq(1,K-1)/K)
  k <- matrix(c(0,rep(k,each=2),Nobs),ncol=2,byrow=TRUE)
  k[,1] <- k[,1]+1
  l <- lapply(seq.int(K),function(x,k,d) 
    list(train=d[!(seq(d) %in% seq(k[x,1],k[x,2]))],
         test=d[seq(k[x,1],k[x,2])]),k=k,d=id)
  return(l)
}