library(dplyr)
library(FNN)
library(caret)
library(e1071)
library(rpart)
library(stringr)
library(randomForest)
library(adabag)
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
                        kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_X')))[i]]))
    y<-data.frame(abs(kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Y')))[i+1]]-
                        kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Y')))[i]]))
    z<-data.frame(abs(kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Z')))[i+1]]-
                        kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Z')))[i]]))
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


type1=kd.df$TYPE

for(j in 1:length(spine)){
  sbxt2<-c()
  sbyt2<-c()
  sbzt2<-c()
  for(i in 1:29){
    x2<-data.frame(((kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_X')))[i+1]]-
                       kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_X')))[i]])^2)*
                     type[,which(str_detect(names(type),paste0(spine[j],'_X')))]/
                     (type[,which(str_detect(names(type),paste0(spine[j],'_X')))]+
                        type[,which(str_detect(names(type),paste0(spine[j],'_Y')))]+
                        type[,which(str_detect(names(type),paste0(spine[j],'_Z')))]))
    y2<-data.frame(((kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Y')))[i+1]]-
                       kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Y')))[i]])^2)*
                     type[,which(str_detect(names(type),paste0(spine[j],'_Y')))]/
                     (type[,which(str_detect(names(type),paste0(spine[j],'_X')))]+
                        type[,which(str_detect(names(type),paste0(spine[j],'_Y')))]+
                        type[,which(str_detect(names(type),paste0(spine[j],'_Z')))]))
    z2<-data.frame(((kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Z')))[i+1]]-
                       kd.df[,which(str_detect(names(kd.df),paste0(spine[j],'_Z')))[i]])^2)*
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
  type1=cbind(type1,sbd)
}


type2=kd.df$TYPE

for(k in 1:length(spine)){
  t<-sqrt(type1[,which(str_detect(names(type1),paste0(spine[k],'_X')))]+
            type1[,which(str_detect(names(type1),paste0(spine[k],'_Y')))]+
            type1[,which(str_detect(names(type1),paste0(spine[k],'_Z')))])
  sbpd=data.frame(t)
  names(sbpd)<-spine[k]
  type2=cbind(type2,sbpd)
}


type3=kd.df$TYPE

for(k in 1:length(spine)){
  t1<-type2[,which(str_detect(names(type2),spine[k]))]*
    (type2[,which(str_detect(names(type2),spine[k]))]/
    apply(type2[,-1],1,sum))
  sdsp=data.frame(t1)
  names(sdsp)<-spine[k]
  type3=cbind(type3,sdsp)
}


train.index<-sample(row.names(type3), 0.7*dim(type3)[1])
valid.index<-setdiff(row.names(type3), train.index)
train.data<-type3[train.index,]
valid.data<-type3[valid.index,]

##부스트 나무 
kd.bs<-boosting(type3 ~ ., data=type3[train.index,],boos = T,mfinal = 100)
kd.bs.predict<-predict(kd.bs, valid.data, type="class")
kd.bs.predict<-as.factor(kd.bs.predict$class)
cobs<-confusionMatrix(kd.bs.predict, valid.data$type3)
cobs
cobs$byClass

