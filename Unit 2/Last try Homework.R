install.packages("ISLR")
install.packages("magrittr")
install.packages("leaps")
library(leaps)
library(magrittr)
library(ISLR)
head(Auto)
## Assigning two variables as categorical variable
Auto$cylinders<-as.factor(Auto$cylinders)
Auto$origin<-as.factor(Auto$origin)
attach(Auto)

summary(Auto)


reg.fwd=regsubsets(log(mpg)~.,data=Auto,method="forward",nvmax=20)

summary(reg.fwd)$adjr2
summary(reg.fwd)$rss
summary(reg.fwd)$bic



##### RSS #########
rss<-summary(reg.fwd)$rss
plot(1:20,rss,type="l",ylab="train RSS",xlab="# of predictors")
index<-which(rss==min(rss))
points(index,rss[index],col="red",pch=10)


##### Split data set into 2 for test and train #####
########### Questions 7.3 ####################
#Auto<-Auto[ , -c(2,9)] ## Remove cylinder and name column
Auto
set.seed(1234) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 50% of data as sample from total 'n' rows of the data  

sample <- sample.int(n = nrow(Auto), size = floor(.50*nrow(Auto)), replace = F)

train <- Auto[sample, ]
test  <- Auto[-sample, ]
#summary(test2)
#summary(train2)

reg.fwd=regsubsets(log(mpg)~.,data=train,method="forward",nvmax=20)




##
#set.seed(1234)
#index<-sample(1:dim(Auto1)[1],100,replace=F)
#train<-Auto1[index,]
#test<-Auto1[-index,]
#reg.fwd=regsubsets(log(mpg)~.,data=train,method="forward",nvmax=20)


#Really handy predict function
predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

testASE<-c()

for (i in 1:20){
  predictions<-predict.regsubsets(object=reg.fwd,newdata=test,id=i) 
  testASE[i]<-mean((log(test$mpg)-predictions)^2)
}
par(mfrow=c(1,1))
plot(1:20,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE")
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss<-summary(reg.fwd)$rss
lines(1:20,rss/196,lty=3,col="blue")  #Dividing by 100 since ASE=RSS/sample size


reg.final=regsubsets(log(mpg)~.,data=Auto,method="forward",nvmax=4)
coef(reg.final,3)

final.model<-lm(log(mpg)~Greens+AvgPutts+Save,data=golf)
summary(final.model)