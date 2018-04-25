setwd("C:/STAT 897D data mining")
# comma delimited data and no header for each variable
RawData <- read.table("diabetes.data",sep = ",",header=FALSE)
AirQualityUCI<- read.csv("C:\Documents and Settings\farzad\Desktop\New Folder\ AirQualityUCI.csv")
AirQualityUCI<- read.csv("C://Documents and Settings/farzad/Desktop/New Folder/AirQualityUCI.csv")
PimaIndians<- read.csv("C://Documents and Settings/farzad/Desktop/New Folder/PimaIndians")
PimaIndians<- read.csv("C://Documents and Settings/farzad/Desktop/New Folder/PimaIndians.csv")
PimaIndians
plot(PimaIndians)
PI=data.frame(PimaIndians)
mm1=glm(Class~.,family=binomial,data=PI)
mm1
summary(mm1)
RPI=PI[,-4] ## dropping triceps skin fold thickness
mm1=glm(Class~.,family=binomial,data=RPI)
mm1
summary(mm1)
RPI=RPI[,-7]
mm1=glm(Class~.,family=binomial,data=RPI)
mm1
summary(mm1)
RPI=RPI[,-4]
RPI[1:3,]
mm1=glm(Class~.,family=binomial,data=RPI)
mm1
summary(mm1)
set.seed(1)
n=length(PI$Class)
n
n1=floor(n*(0.5))
n1
n2=n-n1
n2
train=sample(1:n,n)
PI1=data.frame(PI[train,])
PI2=data.frame(PI[-train,])
mm2=glm(Class~.,family=binomial,data=PI1)
mm2
summary(mm2)
gg=predict(mm2,newdata=PI2,type= "response")
gg
hist(gg)
plot(PI$Class[-train]~gg)
gg1=floor(gg+0.5)
n=predict(mm2,newdata=PI2,type= "response")
mm1=predict(mm2,newdata=PI2,type= "response")
ttt=table(PI$Class[-train],gg1)
ttt
error=(ttt[1,2]+ttt[2,1])/n2
error
mm2=glm(Class~NuPregnancy+Glucose+DiastolicBP+BodyMassIndex+DiabetesPedigree,family=binomial,data=PI1)
mm2
summary(mm2)
gg=predict(mm2,newdata=PI2,type= "response")
gg
hist(gg)
plot(PI$Class[-train]~gg)
PimaIndians<- read.csv("C://Documents and Settings/farzad/Desktop/New Folder/PimaIndians.csv")
library(tree)
utils:::menuInstallPkgs()
library(tree)
PimaIndians$Class=factor(PimaIndians$Class)
PItree <- tree(Class ~., data = PimaIndians,mindev=0.01)
PItree
summary(PItree)
plot(PItree, col=8)
text(PItree, digits=2)
set.seed(2)
cvPI <- cv.tree(PItree, K=10)
cvPI$size
cvPI$dev
plot(cvPI, pch=21, bg=8, type="p", cex=1.5, ylim=c(700,1000))
PIcut <- prune.tree(PItree, best=7)
PIcut
summary(PIcut)
plot(PIcut, col=8)
text(PIcut)
P1=snip.tree(PIcut,nodes=c(2,7))
P1
summary(P1)
plot(P1)
text(P1)
plot