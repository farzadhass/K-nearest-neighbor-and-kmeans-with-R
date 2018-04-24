n = 1000
kk = 10    
x1 = runif(kk)
y1 = runif(kk)
z1 = runif(kk)    
x4 = sample(x1,length(x1))
y4 = sample(y1,length(y1)) 
randObs <- function()
{
  ix = sample( 1:length(x4), 1 )
  iy = sample( 1:length(y4), 1 )
  rx = rnorm( 1, x4[ix], runif(1)/8 )
  ry = rnorm( 1, y4[ix], runif(1)/8 )
  return( c(rx,ry) )
}  
x = c()
y = c()
for ( k in 1:n )
{
  rPair  =  randObs()
  x  =  c( x, rPair[1] )
  y  =  c( y, rPair[2] )
}
z <- rnorm(n)
d <- data.frame( x, y, z )
g = 6 
set.seed(g)
d <- data.frame(x = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))), 
                y = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))))
plot(d)
library(mclust)
# Run the function to see how many clusters
# it finds to be optimal, set it to search for
# at least 1 model and up 20.
d_clust <- Mclust(as.matrix(d), G=1:20)
m.best <- dim(d_clust$z)[2]
cat("model-based optimal number of clusters:", m.best, "\n")
# 4 clusters
plot(d_clust)
# set the working directory
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
 PimaIndians<- read.csv("C://Documents and Settings/farzad/Desktop/New Folder/PimaIndians.csv")
plot(PimaIndians)
PI=data.frame(PimaIndians)
mm1=glm(Class~.,family=binomial,data=PI)
mm1
summary(mm1)
RPI=PI[,-4]
mm1=glm(Class~.,family=binomial,data=RPI)
mm1
summery(mm1)
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
train=sample(1:n,n1)
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
gg1=floor(gg+0.5)
ttt=table(PI$Class[-train],gg1)
ttt
error=(ttt[1,2]+ttt[2,1])/n2
error
 PimaIndians<- read.csv("C://Documents and Settings/farzad/Desktop/New Folder/PimaIndians.csv")
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
plot(p1)
plot(P1)
text(P1)
q()
library(textir) ## needed to standardize the data 
library(class) ## needed for knn 
credit <- read.csv("G://DataMining/germancredit.csv")
credit$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) = c("good","good","poor","poor","terrible")
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), labels=c("foreign","german"))
credit$rent <- factor(credit$housing=="A151")
credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")
credit <- credit[,c("Default","duration","amount","installment","age", "history", "purpose","foreign","rent")]
credit[1:3,]
summary(credit)
## x <- normalize(credit[,c(2,3,4)])
x=credit[,c(2,3,4)]
x[,1]=(x[,1]-mean(x[,1]))/sd(x[,1])
x[,2]=(x[,2]-mean(x[,2]))/sd(x[,2])
x[,3]=(x[,3]-mean(x[,3]))/sd(x[,3])
x[1:3,]
set.seed(1)
train <- sample(1:1000,900) ## this is training set of 900 borrowers
xtrain <- x[train,]
xnew <- x[-train,]
ytrain <- credit$Default[train]
ynew <- credit$Default[-train]
## k-nearest neighbor method
library(class)
nearest1 <- knn(train=xtrain, test=xnew, cl=ytrain, k=1)
nearest3 <- knn(train=xtrain, test=xnew, cl=ytrain, k=3)
data.frame(ynew,nearest1,nearest3)[1:10,]
## calculate the proportion of correct classifications
pcorrn1=100*sum(ynew==nearest1)/100
pcorrn3=100*sum(ynew==nearest3)/100
pcorrn1
pcorrn3
## plot for 3nn
plot(xtrain[,c("amount","duration")],col=c(4,3,6,2)[credit[train,"installment"]],pch=c(1,2)[as.numeric(ytrain)],main="Predicted default, by 3 nearest neighbors",cex.main=.95)
points(xnew[,c("amount","duration")],bg=c(4,3,6,2)[credit[train,"installment"]],pch=c(21,24)[as.numeric(nearest3)],cex=1.2,col=grey(.7))
legend("bottomright",pch=c(1,16,2,17),bg=c(1,1,1,1),legend=c("data 0","pred 0","data 1","pred 1"),title="default",bty="n",cex=.8)
legend("topleft",fill=c(4,3,6,2),legend=c(1,2,3,4),title="installment %",horiz=TRUE,bty="n",col=grey(.7),cex=.8)
## above was for just one training set
## cross-validation (leave one out)
pcorr=dim(10)
for (k in 1:10) {
pred=knn.cv(x,cl=credit$Default,k)
pcorr[k]=100*sum(credit$Default==pred)/1000
}
pcorr
