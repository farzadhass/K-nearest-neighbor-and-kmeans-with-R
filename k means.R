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
 wine <- read.csv("G://DataMining/wine.csv")
wine[1:3,]
plot(wine)
library(tree)
wine$Class=factor(wine$Class)
Winetree <- tree(Class ~., data = wine)
Winetree
summary(Winetree)
plot(Winetree, col=8)
text(Winetree, digits=2)
set.seed(1)
cvWine <- cv.tree(Winetree, K=10)
cvWine$size
cvWine$dev
plot(cvWine, pch=21, bg=8, type="p", cex=1.5, ylim=c(100,400))
Winecut <- prune.tree(Winetree, best=4)
Winecut
summary(Winecut)
plot(Winecut, col=8)
text(Winecut)
## Clustering
## standardizing the attributes as units considerably different
wines=matrix(nrow=length(wine[,1]),ncol=length(wine[1,]))
for (j in 2:14) {
wines[,j]=(wine[,j]-mean(wine[,j]))/sd(wine[,j])
}
wines[,1]=wine[,1]
winesr=wines[,-1]
winesr[1:3,]
## kmeans clustering with 13 standardized attributes
grpwines <- kmeans(winesr, centers=3, nstart=20)
grpwines
grpwines$cluster
wine$Class ## actual classes
## 6 mistakes made among 178 wines
library(MASS)
wines[1:3,]
ws=data.frame(wines)
ws[1:3,]
zlin=lda(X1~.,ws,prior=c(1,1,1)/3)
zlin
## quadratic discriminant analysis
zqua=qda(X1~.,ws,prior=c(1,1,1)/3)
zqua
n=dim(ws)[1]
errorlin=1-(sum(ws$X1==predict(zlin,ws)$class)/n)
errorlin
errorqua=1-(sum(ws$X1==predict(zqua,ws)$class)/n)
errorqua
neval=1
corlin=dim(n)
corqua=dim(n)
## leave one out evaluation
for (k in 1:n) {
train1=c(1:n)
train=train1[train1!=k]
zlin=lda(X1~.,ws[train,],prior=c(1,1,1)/3)
corlin[k]=ws$X1[-train]==predict(zlin,ws[-train,])$class
zqua=qda(X1~.,ws[train,],prior=c(1,1,1)/3)
corqua[k]=ws$X1[-train]==predict(zqua,ws[-train,])$class
}
merrlin=1-mean(corlin)
merrlin
merrqua=1-mean(corqua)
merrqua
## quadratic discriminant analysis past structure
library(VGAM)
setRepositories()
utils:::menuInstallPkgs()
library(VGAM)
ws=data.frame(wines)
gg <- vglm(X1 ~ .,multinomial,data=ws)
summary(gg)
predict(gg)
round(fitted(gg),2) ## probabilities
cbind(round(fitted(gg),2),ws$X1)
## perfect classification
