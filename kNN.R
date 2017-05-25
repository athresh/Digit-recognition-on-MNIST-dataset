#kNN
#Load required libraries
library(ggplot2)
library(lattice)
library(caret)
library(nnet)
library(class)
training<-read.csv("train.csv")
inTrain<-sample(1:42000,35000)     # 42000 examples are too many for now. Can use all later.
trainSub<-training[inTrain,]
#trainSub<-training

scaling<-function(f){
  (f-sum(f)/(dim(f)[1]*dim(f)[2]))/(max(f)-min(f))
}

trainSub[2:785]<-scaling(trainSub[2:785])   #Mean normalization and feature scaling applied to training and test sets

nzv<-nearZeroVar(trainSub[2:785],saveMetrics = TRUE)   #remove variables with near zero variance
trainSub_nzv<- trainSub[c(rownames(nzv[nzv$percentUnique>0.1,]))]
trainSub_nzv<-cbind(label=trainSub$label,trainSub_nzv)


testing<-read.csv("test.csv")
testing<-scaling(testing)
testing_nzv<- testing[c(rownames(nzv[nzv$percentUnique>0.1,]))]

#preparing the crossvalidation set
incv<-sample(setdiff(1:42000,inTrain),5000)
cvset<-training[incv,]
cvset[2:785]<-scaling(cvset[2:785])
cvset_nzv<-cvset[c(rownames(nzv[nzv$percentUnique>0.1,]))]
cvset_nzv<-cbind(label=cvset$label,cvset_nzv)

numiter=10
accuracycv<-rep(0,numiter)
iter=1:numiter
#mdl_test<- knn(trainSub_nzv[2:598],testing_nzv[2:598],trainSub_nzv$label,k=5)
for(i in 1:numiter){
  mdl_cv<- knn(trainSub_nzv[2:dim(trainSub_nzv)[2]],cvset_nzv[2:dim(cvtest_nzv)[2]],trainSub_nzv$label,k=i)
  accuracycv[i]<-sum(mdl_cv==cvset_nzv$label)/dim(cvset_nzv)[1]
}
p<-ggplot(data.frame(iter,accuracycv),aes(iter,accuracycv))+geom_point()
p

mdl_test<- knn(trainSub_nzv[2:dim(trainSub_nzv)[2]],testing_nzv,trainSub_nzv$label,k=which.max(accuracycv))
write.csv(results,file="submission_knn.csv",row.names=FALSE)
