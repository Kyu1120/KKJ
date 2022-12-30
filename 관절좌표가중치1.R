library(dplyr)
library(FNN)
library(caret)
library(e1071)
library(rpart)
library(stringr)
kd.df<-read.csv("project.csv", stringsAsFactors = T)
str(kd.df)
set.seed(32180377)

spine<-c("SpineBase","SpineMid","Neck","Head","ShoulderLeft",    
         "ElbowLeft","WristLeft","HandLeft","ShoulderRight","ElbowRight",
         "WristRight","HandRight","HipLeft","KneeLeft","AnkleLeft",
         "FootLeft","HipRight","KneeRight","AnkleRight","FootRight",
         "SpineShoulder","HandTipLeft","ThumbLeft","HandTipRight","ThumbRight")

type=kd.df$TYPE # y

for(j in 1:length(spine)){
  sbxt<-c()
  sbyt<-c()
  sbzt<-c()
  
  for(i in 1:29){
    x<-data.frame(abs(kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_X')))[i+1]]-
                        kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_X')))[1]]))
    y<-data.frame(abs(kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Y')))[i+1]]-
                        kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Y')))[1]]))
    z<-data.frame(abs(kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Z')))[i+1]]-
                        kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Z')))[1]]))
    sbxt<-c(sbxt,x)
    sbyt<-c(sbyt,y)
    sbzt<-c(sbzt,z)
  }
  sbxt=apply(data.frame(sbxt),1,sum) 
  sbyt=apply(data.frame(sbyt),1,sum)  
  sbzt=apply(data.frame(sbzt),1,sum) 
  s=data.frame(sbxt,sbyt,sbzt)
  names(s)<-c(paste0(spine[j],'_X'),paste0(spine[j],'_Y'),paste0(spine[j],'_Z'))
  type=cbind(type,s)
}
View(type)

type1<-kd.df$TYPE
for(j in 1:length(spine)){
  sbxt2<-c()
  sbyt2<-c()
  sbzt2<-c()
  for(i in 1:29){
    x2<-data.frame(((kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_X')))[i+1]]-
                       kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_X')))[1]])^2)*
                     type[,which(str_detect(names(type),paste0(spine[j],'_X')))]/
                     (type[,which(str_detect(names(type),paste0(spine[j],'_X')))]+
                        type[,which(str_detect(names(type),paste0(spine[j],'_Y')))]+
                        type[,which(str_detect(names(type),paste0(spine[j],'_Z')))]))
    y2<-data.frame(((kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Y')))[i+1]]-
                       kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Y')))[1]])^2)*
                     type[,which(str_detect(names(type),paste0(spine[j],'_Y')))]/
                     (type[,which(str_detect(names(type),paste0(spine[j],'_X')))]+
                        type[,which(str_detect(names(type),paste0(spine[j],'_Y')))]+
                        type[,which(str_detect(names(type),paste0(spine[j],'_Z')))]))
    z2<-data.frame(((kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Z')))[i+1]]-
                       kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Z')))[1]])^2)*
                     type[,which(str_detect(names(type),paste0(spine[j],'_Z')))]/
                     (type[,which(str_detect(names(type),paste0(spine[j],'_X')))]+
                        type[,which(str_detect(names(type),paste0(spine[j],'_Y')))]+
                        type[,which(str_detect(names(type),paste0(spine[j],'_Z')))]))
    sbxt2<-c(sbxt2,x2)
    sbyt2<-c(sbyt2,y2)
    sbzt2<-c(sbzt2,z2)
  }
  sbxt2=apply(data.frame(sbxt2),1,sum) 
  sbyt2=apply(data.frame(sbyt2),1,sum)  
  sbzt2=apply(data.frame(sbzt2),1,sum) 
  sbd=data.frame(sbxt2,sbyt2,sbzt2)
  names(sbd)<-c(paste0(spine[j],'_X'),paste0(spine[j],'_Y'),paste0(spine[j],'_Z'))
  type1<-cbind(type1,sbd)
}
View(type1)

type2=kd.df$TYPE

for(k in 1:length(spine)){
  t<-sqrt(type1[,which(str_detect(names(type1),paste0(spine[k],'_X')))]+
            type1[,which(str_detect(names(type1),paste0(spine[k],'_Y')))]+
            type1[,which(str_detect(names(type1),paste0(spine[k],'_Z')))])
  sbpd=data.frame(t)
  names(sbpd)<-spine[k]
  type2<-cbind(type2,sbpd)
}
View(type2)

train.index<-sample(row.names(type2), 0.7*dim(type2)[1])
valid.index<-setdiff(row.names(type2), train.index)
train.data<-type2[train.index,]
valid.data<-type2[valid.index,]


##knn모델    
kd.nn<-knn(train=train.data[-1], test=valid.data[-1], cl=type2[train.index,1],k=61)
kd.nn
summary(kd.nn)
co<-confusionMatrix(kd.nn, type2[valid.index,1],positive = "walk")
co$byClass

##나이브 베이즈 모델
kd.nb<-naiveBayes(train.data, type2[train.index,1],laplace=1)
kd.nb.predict<-predict(kd.nb, valid.data)
summary(kd.nb.predict)
co1<-confusionMatrix(kd.nb.predict, type2[valid.index,1])
co$byClass

##의사 결정 나무
kd.tree<-rpart(type2 ~ .,data=type2[train.index,], method = "class")
kd.tree.predict<-predict(kd.tree, valid.data, type = "class")
summary(kd.tree.predict)
co3<-confusionMatrix(kd.tree.predict, type2[valid.index,1])
co3$byClass
