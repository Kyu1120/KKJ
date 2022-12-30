library(dplyr)
library(FNN)
library(caret)
library(e1071)
library(rpart)
library(stringr)
library(randomForest)
library(adabag)
library(neuralnet)
library(nnet)


#df불러오기
df<-read.csv("train.csv")
dft<-read.csv("test.csv")
dftt<-read.csv("따릉이 21년도 데이터.csv")
dftt

##df변수변환
df<-df%>%mutate(precipitation=ifelse(is.na(precipitation),0,precipitation))
df<-df%>%mutate(PM10=ifelse(is.na(PM10),0,PM10))
df<-df%>%mutate(PM2.5=ifelse(is.na(PM2.5),0,PM2.5))
df<-df%>%mutate(sunshine_sum=ifelse(is.na(sunshine_sum),0,sunshine_sum))
df$PM10<-as.numeric(df$PM10)
df$PM2.5<-as.numeric(df$PM2.5)

dft<-dft%>%mutate(precipitation=ifelse(is.na(precipitation),0,precipitation))
dft<-dft%>%mutate(PM10=ifelse(is.na(PM10),0,PM10))
dft<-dft%>%mutate(PM2.5=ifelse(is.na(PM2.5),0,PM2.5))
dft<-dft%>%mutate(sunshine_sum=ifelse(is.na(sunshine_sum),0,sunshine_sum))

df$precipitationw<-ifelse(df$precipitation>60,1,0)
df$precipitationw <- as.factor(df$precipitationw)
df$temp_lowestw<-ifelse(df$temp_lowest<=-12,1,0)
df$temp_lowestw <- as.factor(df$temp_lowestw)
df$temp_highestw<-ifelse(df$temp_highest>=33,1,0)
df$temp_highestw <- as.factor(df$temp_highestw)

dft$precipitationw<-ifelse(dft$precipitation>60,1,0)
dft$temp_lowestw<-ifelse(dft$temp_lowest<=-12,1,0)
dft$temp_highestw<-ifelse(dft$temp_highest>=33,1,0)

##train 파생변수
tw<-function(x,y){
  x*atan(0.151977*((y+8.313659)**(1/2))) + atan(x+y) -
    atan(y-1.67633) + 0.00391838*(y**(3/2))*atan(0.023101*y) - 4.686035
}
df$tw <- tw(df$temp_mean, df$humidity)
df$tw

ftmax<-function(x,y){
  (-0.2442) + 0.55399*x + 0.45535*y - 0.0022*(x**2) +
    0.00278*x*y + 3.0
}
df$ftmax <- ftmax(df$tw, df$temp_highest)
df$ftmax
options(scipen = 9999)

ftmin <- function(x,y){
  13.12 + 0.6215*x - 11.37*(y**(16/100)) + 0.3965*(y**(16/100))*x
}
df$ftmin <- ftmin(df$temp_lowest, df$wind_mean)
df$ftmin

di <- function(x,y){
  0.72*(x+y) +40.6
}
df$di <- di(df$temp_mean, df$tw)
df$di

##test 파생변수
tw<-function(x,y){
  x*atan(0.151977*((y+8.313659)**(1/2))) + atan(x+y) -
    atan(y-1.67633) + 0.00391838*(y**(3/2))*atan(0.023101*y) - 4.686035
}
dft$tw <- tw(dft$temp_mean, dft$humidity)
dft$tw

ftmax<-function(x,y){
  (-0.2442) + 0.55399*x + 0.45535*y - 0.0022*(x**2) +
    0.00278*x*y + 3.0
}
dft$ftmax <- ftmax(dft$tw, dft$temp_highest)
dft$ftmax
options(scipen = 9999)

ftmin <- function(x,y){
  13.12 + 0.6215*x - 11.37*(y**(16/100)) + 0.3965*(y**(16/100))*x
}
dft$ftmin <- ftmin(dft$temp_lowest, dft$wind_mean)
dft$ftmin

dft$di <- di(dft$temp_mean, dft$tw)

##미세먼지 결측치 제거
jongro2018<-read.csv("기간별_일평균_대기환경_정보_2018년_종로구.csv", fileEncoding = "euc-kr")
jongro2019<-read.csv("기간별_일평균_대기환경_정보_2019년_종로구.csv", fileEncoding = "euc-kr")
jongro2020<-read.csv("기간별_일평균_대기환경_정보_2020년_종로구.csv", fileEncoding = "euc-kr")

jongro2018<-jongro2018[,c(1,6,7)]
jongro2019<-jongro2019[,c(1,6,7)]
jongro2020<-jongro2020[,c(1,6,11)]
View(df)
PM<-rbind(jongro2018, jongro2019, jongro2020)
names(PM)<-c("date", "PM10", "PM2.5")
PM$date <- as.Date(as.character(PM$date), format='%Y%m%d')
PM$date <- as.character(PM$date)

dfpm<-left_join(df, PM, 'date')
View(dfpm)
df$PM10<-dfpm$PM10.y
df$PM2.5<-dfpm$PM2.5.y
sum(is.na(df$PM10))
sum(is.na(df$PM2.5))
View(df)
df.PM10.02<-df[which(str_detect(df$date, "-02-")),6]
df.PM10.03<-df[which(str_detect(df$date, "-03-")),6]
df.PM2.5.02<-df[which(str_detect(df$date, "-02-")),7]
df.PM2.5.03<-df[which(str_detect(df$date, "-03-")),7]
round(mean(df.PM10.02, na.rm=TRUE))
round(mean(df.PM10.03, na.rm=TRUE))
round(mean(df.PM2.5.02, na.rm=TRUE))
round(mean(df.PM2.5.03, na.rm=TRUE))
df.PM10.02<-ifelse(is.na(df.PM10.02),round(mean(df.PM10.02, na.rm=TRUE)),df.PM10.02)
df.PM10.03<-ifelse(is.na(df.PM10.03),round(mean(df.PM10.03, na.rm=TRUE)),df.PM10.03)
df.PM2.5.02<-ifelse(is.na(df.PM2.5.02),round(mean(df.PM2.5.02, na.rm=TRUE)),df.PM2.5.02)
df.PM2.5.03<-ifelse(is.na(df.PM2.5.03),round(mean(df.PM2.5.03, na.rm=TRUE)),df.PM2.5.03)

df[which(str_detect(df$date, "-02-")),6]<-df.PM10.02
df[which(str_detect(df$date, "-03-")),6]<-df.PM10.03
df[which(str_detect(df$date, "-02-")),7]<-df.PM2.5.02
df[which(str_detect(df$date, "-03-")),7]<-df.PM2.5.03
View(df)

##렌탈 변수에 보정
sum(df[which(str_detect(df$date, "2018")),13])
sum(df[which(str_detect(df$date, "2019")),13])
sum(df[which(str_detect(df$date, "2020")),13])
mem<-read.csv("년도별 이용자수 현황.csv")
str(bike)
str(mem)
train.index<-mem[-7,]
valid.index<-mem[7,]
mem.lm<-lm(member ~ date + bike, data=train.index)
used.lm<-lm(used ~ date + bike, data=train.index)
mem.lm.pd<-predict(mem.lm, valid.index)
used.lm.pd<-predict(used.lm, valid.index)
used.lm.pd
mem.lm.pd
used.lm.pd/mem[4,3]
used.lm.pd/mem[5,3]
used.lm.pd/mem[6,3]
df[,21]<-df$corrental
df[which(str_detect(df$date, "2018"), df$precipitation < 50) ,13]<-df[which(str_detect(df$date, "2018"), df$precipitation < 50) ,13]*used.lm.pd/mem[4,3]
df[which(str_detect(df$date, "2019"), df$precipitation < 50) ,13]<-df[which(str_detect(df$date, "2019"), df$precipitation < 50) ,13]*used.lm.pd/mem[5,3]
df[which(str_detect(df$date, "2020"), df$precipitation < 50) ,13]<-df[which(str_detect(df$date, "2020"), df$precipitation < 50) ,13]*used.lm.pd/mem[6,3]

View(df)

comem<-confusionMatrix(mem.lm.pd, valid.index$member)

## 일별, 월별, 요일별 변수

df$date_n <- as.Date(df$date)
df$date_y <- 0
df$date_y <- as.factor(format(df$date_n,'%Y'))
df$date_m <- 0
df$date_m <- as.factor(format(df$date_n,'%m'))
df$date_d <- 0
df$date_d <- as.factor(format(df$date_n,'%d'))
df$date_w <- 0
df$date_w <- as.factor(format(df$date_n,'%w'))

dft$date_n<- as.Date(dft$date)
dft$date_y <- 0
dft$date_y <- as.factor(format(dft$date_n,'%Y'))
dft$date_m <- 0
dft$date_m <- as.factor(format(dft$date_n,'%m'))
dft$date_d <- 0
dft$date_d <- as.factor(format(dft$date_n,'%d'))
dft$date_w <- 0
dft$date_w <- as.factor(format(dft$date_n,'%w'))



##NMAE
nmae<-function(x,y) {
  return (mean(abs(y-x)/x))
}
nmae(dftt$rental, p)

View(df)
str(df)

##신경망 정규화하기
df$precipitation <- (df$precipitation - min(df$precipitation))/(max(df$precipitation) - min(df$precipitation))
df$temp_mean <- (df$temp_mean - min(df$temp_mean))/(max(df$temp_mean) - min(df$temp_mean))
df$temp_highest <- (df$temp_highest - min(df$temp_highest))/(max(df$temp_highest) - min(df$temp_highest))
df$temp_lowest <- (df$temp_lowest - min(df$temp_lowest))/(max(df$temp_lowest) - min(df$temp_lowest))
df$PM10 <- (df$PM10 - min(df$PM10))/(max(df$PM10) - min(df$PM10))
df$PM2.5 <- (df$PM2.5 - min(df$PM2.5))/(max(df$PM2.5) - min(df$PM2.5))
df$humidity <- (df$humidity - min(df$humidity))/(max(df$humidity) - min(df$humidity))
df$sunshine_sum <- (df$sunshine_sum - min(df$sunshine_sum))/(max(df$sunshine_sum) - min(df$sunshine_sum))
df$sunshine_rate <- (df$sunshine_rate - min(df$sunshine_rate))/(max(df$sunshine_rate) - min(df$sunshine_rate))
df$wind_mean <- (df$wind_mean - min(df$wind_mean))/(max(df$wind_mean) - min(df$wind_mean))
df$wind_max <- (df$wind_max - min(df$wind_max))/(max(df$wind_max) - min(df$wind_max))
df$rental <- (df$rental - min(df$rental))/(max(df$rental) - min(df$rental))
df$tw <- (df$tw - min(df$tw))/(max(df$tw) - min(df$tw))
df$ftmax <- (df$ftmax - min(df$ftmax))/(max(df$ftmax) - min(df$ftmax))
df$ftmin <- (df$ftmin - min(df$ftmin))/(max(df$ftmin) - min(df$ftmin))
df$di <- (df$di - min(df$di))/(max(df$di) - min(df$di))
summary(df)

##신경망 변수 선택
training<-sample(row.names(df), dim(df)[1]*0.7)
validation<-setdiff(row.names(df), training)
vars <- c( "precipitation",
           "PM10", "PM2.5", "sunshine_sum", "sunshine_rate", "wind_mean",
           "wind_max", "rental", "ftmax", "ftmin", "di")
set.seed(22)
class.ind <- function(cl){
  n<-length(cl)
  cl <- as.factor(cl)
  x <- matrix(0, n , length(levels(cl)))
  x[(1:n) + n*(unclass(cl)-1)] <- 1
  dimnames(x) <- list(names(cl), levels(cl))
  x
}
str(df)


##신경망 변수 더미화
nndft <- cbind(df[training,c(vars)],
               class.ind(df[training,]$precipitationw),
               class.ind(df[training,]$temp_lowestw),
               class.ind(df[training,]$temp_highestw),
               class.ind(df[training,]$date_y),
               class.ind(df[training,]$date_m),
               class.ind(df[training,]$date_d),
               class.ind(df[training,]$date_w))

names(nndft) <- c(vars,
                  paste("precipitaionw" , c(0,1), sep=""),
                  paste("temp_lowestw" , c(0,1), sep=""),
                  paste("temp_highestw" , c(0,1), sep=""),
                  paste("date_y" , c(2018, 2019, 2020), sep=""),
                  paste("date_m" , c(1:12), sep=""),
                  paste("date_d" , c(1:31), sep=""),
                  paste("date_w" , c(0:6), sep=""))
View(nndft)
colnames(nndft)
nndfv <- cbind(df[validation,c(vars)],
               class.ind(df[validation,]$precipitationw),
               class.ind(df[validation,]$temp_lowestw),
               class.ind(df[validation,]$temp_highestw),
               class.ind(df[validation,]$date_y),
               class.ind(df[validation,]$date_m),
               class.ind(df[validation,]$date_d),
               class.ind(df[validation,]$date_w))

names(nndfv) <- c(vars,
                  paste("precipitaionw" , c(0,1), sep=""),
                  paste("temp_lowestw" , c(0,1), sep=""),
                  paste("temp_highestw" , c(0,1), sep=""),
                  paste("date_y" , c(2018, 2019, 2020), sep=""),
                  paste("date_m" , c(1:12), sep=""),
                  paste("date_d" , c(1:31), sep=""),
                  paste("date_w" , c(0:6), sep=""))

##신경망
nn <- neuralnet(rental ~ ., data=nndft,
                algorithm = 'rprop+',
                hidden=c(10,3),
                threshold = 0.1,
                stepmax = 1e+06,
                linear.output = T
                )
plot(nn)
model_results <- neuralnet::compute(nn, nndfv[,-nndfv$rental])
model_results$net.result
cor(model_results$net.result, nndfv$rental)


training.pd<-predict(nn, nndfv)
length(training.pd)
length(nndfv$rental)


td<-df[train,]
vd<-df[-train,]
rm(df, train)

colnames(td)
library(neuralnet)
set.seed(1)
NN = neuralnet(rental ~ , td, linear.output = F, hidden=3)

##nnet
library(neuralnet)
library(caret)

set.seed(123)


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
normalize1 <- function(x) {
  return ((x * (max(df1$rental) - min(df1$rental)))+min(df1$rental))
}
df11 <- as.data.frame(lapply(df[,-1], normalize))
dft11 <- as.data.frame(lapply(dft[,-1], normalize))

nn<-neuralnet(rental~.,data = df11,hidden = 5, linear.output = TRUE)

fit<-neuralnet(rental~., 
               data = df11,
               hidden = 5,
               err.fct ="sse",
               act.fct = "logistic",
               linear.output = TRUE)
pred<-predict(nn,dft11)
pred<-normalize1(pred)
pred<-as.data.frame(pred)
pred$V1<-as.numeric(pred$V1)
caret::RMSE(pred$V1,dftt$rental)




library(nnet)

model.nnet <- nnet(rental ~ ., data = df11, size = 1, decay = 5e-04)

pre<-predict(model.nnet,dft11)

pre<-normalize1(pre)
pre<-as.data.frame(pre)
pre$V1<-as.numeric(pre$V1)
caret::RMSE(pre$V1,dftt$rental)


mm<-lm(rental~.,data = df1)
summary(mm)
mmp<-predict(mm,dft1)
caret::RMSE(mmp,dftt$rental)

## 랜포변수선택
colnames(df)
colnames(dft)

df1<-df[,c(2,6,7,13,18,19,20,21,22,23,24,25)]
dft1<-dft[,c(2,6,7,17,18,19,20,21,22,23,24)]

##랜포변수 분할
idx <- sample(1:nrow(df1), size = nrow(df1)*0.7, replace=FALSE)
train <- df1[idx,]
test <- df1[-idx,]


rf.fit = randomForest(rental~.,data = df1,mtry=5,ntree=1000,importance=T)
varImpPlot(rf.fit)

p<-predict(rf.fit, dft1)
View(p)
View(round(p))
p<-round(p)
caret::RMSE(p,dftt$rental)
dftt<-as.data.frame(p)
write.csv(dftt,file="pred.csv")



