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

summary(Auto1)


reg.fwd=regsubsets(log(mpg)~.,data=Auto1,method="forward",nvmax=20)

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
Auto1=Auto1[ , -c(2,8)] ## Remove cylinder and name column

##set.seed(1234) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 50% of data as sample from total 'n' rows of the data  

#sample <- sample.int(n = nrow(Auto2), size = floor(.50*nrow(Auto2)), replace = F)

#train2 <- Auto2[sample, ]
#test2  <- Auto2[-sample, ]
#summary(test2)
#summary(train2)

#reg.fwd2=regsubsets(log(mpg)~.,data=train2,method="forward",nvmax=20)




set.seed(1234)
index<-sample(1:dim(Auto1)[1],100,replace=F)
train<-Auto1[index,]
test<-Auto1[-index,]
reg.fwd=regsubsets(log(mpg)~.,data=train,method="forward",nvmax=20)

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
plot(1:20,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE"))
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red",pch=10)
rss<-summary(reg.fwd)$rss
lines(1:20,rss/100,lty=3,col="blue")  #Dividing by 100 since ASE=RSS/sample size


reg.final=regsubsets(log(AvgWinnings)~.,data=golf,method="forward",nvmax=4)
coef(reg.final,3)

final.model<-lm(log(AvgWinnings)~Greens+AvgPutts+Save,data=golf)
summary(final.model)

plot(exp(final.model$fitted.values),golf$AvgWinnings,xlab="Predicted",ylab="AvgWinnings",xlim=c(0,400000),ylim=c(0,400000))
lines(c(0,400000),c(0,400000),col="red")

head(predict(final.model,golf,interval="predict"))


##########################################################################
################### Question 7 ###########################################

Auto1 = Auto[Auto$cylinders !=3 & Auto$cylinders!=5, ] # New variable without Cylinders = 3 an 5
summary(Auto1)

set.seed(1234) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 50% of data as sample from total 'n' rows of the data  
index <- sample.int(n = nrow(Auto1), size = floor(.50*nrow(Auto1)), replace = F)
train <- Auto1[index, ]
test  <- Auto1[-index, ]
summary(test)
summary(train)


########### Questions 7.3 ####################
Auto1=Auto1[ , -c(2)] ## Remove cylinder row

set.seed(1234) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 50% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(Auto2), size = floor(.50*nrow(Auto2)), replace = F)
train2 <- Auto2[sample, ]
test2  <- Auto2[-sample, ]
summary(test2)
summary(train2)

reg.fwd2=regsubsets(log(mpg)~.,data=train2,method="forward",nvmax=20)
testASE2<-c()
#note my index is to 20 since that what I set it in regsubsets
for (i in 1:20){
  predictions2<-predict.regsubsets(object=reg.fwd2,newdata=test2,id=i) 
  testASE2[i]<-mean((log(test2$mpg)-predictions2)^2)
}
par(mfrow=c(1,1))
plot(1:20,testASE2,type="l",xlab="# of predictors",ylab="test vs train ASE",ylim=c(0.3,0.8))
index<-which(testASE2==min(testASE2))
points(index,testASE2[index],col="red",pch=10)
rss2<-summary(reg.fwd2)$rss
lines(1:20,rss2/389,lty=3,col="blue")  #Dividing by 100 since ASE=RSS/sample size






