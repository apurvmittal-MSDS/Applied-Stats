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

##For categorical explanatory variables it can be helpful to view summary statistics by each categorical level. 
##Examining cylinders below we can quanitfy the shifts a little better using means. 
t(aggregate(mpg~cylinders,data=Auto,summary))

t(aggregate(mpg~cylinders,data=Auto,mean)) #to just get means of each cylinder
t(aggregate(mpg~cylinders,data=Auto,sd)) #to just get standard deviations of each cylinder


##explore how the potential explanatory variables may be correlated to the response.
par(mfrow=c(1,3))

## Scatterplot Horespower vs MPG
plot(horsepower,mpg, xlab="horsepower",ylab="mpg")
new<-data.frame(horsepower=seq(30,300,.1))
lines(seq(30,300,.1),predict(lm(mpg~horsepower),newdata=new),col="red",lwd=4)

## Box plot between Cylinders and MPG
plot(cylinders, mpg,xlab="cylinders",ylab="mpg",title="Auto Data Set",col=c(7,32,57,82,107))

## Scatterplot Weight vs MPG
plot(weight,mpg)
new2<-data.frame(weight=seq(1600,5200,1))
lines(seq(1600,5200,1),predict(lm(mpg~weight),newdata=new2),col="red",lwd=4)

### Scatterplot Matrix (All Rows and all columns except 2 and 9)

pairs(Auto[,-c(2,9)],col=cylinders)

### VIF

library(car)     #where vif function lives
Auto<-Auto[,-9]  #removing the car name
full.model<-lm(mpg~.,data=Auto)  # . means all variable not mpg
vif(full.model)[,3]^2

### 
par(mfrow=c(1,3))
plot(horsepower,mpg, xlab="horsepower",ylab="mpg")
new<-data.frame(horsepower=seq(30,300,.1))
horse.model<-lm(mpg~horsepower)
lines(seq(30,300,.1),predict(horse.model,newdata=new),col="red",lwd=4)
plot(horse.model$fitted.values,horse.model$residuals,xlab="Fitted Values",ylab="Residuals")
plot(horsepower,horse.model$residuals,xlab="Horsepower",ylab="Residuals")

###### Quadratic transformation

par(mfrow=c(1,3))
plot(horsepower,mpg, xlab="horsepower",ylab="mpg")
new<-data.frame(horsepower=seq(30,300,.1))
horse.model2<-lm(mpg~horsepower+I(horsepower^2))
lines(seq(30,300,.1),predict(horse.model2,newdata=new),col="red",lwd=4)
plot(horse.model2$fitted.values,horse.model2$residuals,xlab="Fitted Values",ylab="Residuals")
plot(horsepower,horse.model2$residuals,xlab="Horsepower",ylab="Residuals")

###basic residual diagnostic plot
par(mfrow=c(2,2))
plot(horse.model2)

summary(horse.model2)
#### Question 6

### Log MPG and Horsepower Quadratic

par(mfrow=c(1,3))
plot(horsepower,log(mpg), xlab="horsepower",ylab="Log of mpg")
new<-data.frame(horsepower=seq(30,300,.1))
horse.model3<-lm(log(mpg)~horsepower+I(horsepower^2))
lines(seq(30,300,.1),predict(horse.model3,newdata=new),col="red",lwd=4)
plot(horse.model3$fitted.values,horse.model3$residuals,xlab="Fitted Values",ylab="Residuals")
plot(horsepower,horse.model3$residuals,xlab="Horsepower",ylab="Residuals")

par(mfrow=c(2,2))
plot(horse.model3)

summary(horse.model3)


### Log MPG and Horsepower ---No Quadratic

par(mfrow=c(1,3))
plot(horsepower,log(mpg), xlab="horsepower",ylab="Log of mpg")
new<-data.frame(horsepower=seq(30,300,.1))
horse.model4<-lm(log(mpg)~horsepower)
lines(seq(30,300,.1),predict(horse.model4,newdata=new),col="red",lwd=4)
plot(horse.model4$fitted.values,horse.model4$residuals,xlab="Fitted Values",ylab="Residuals")
plot(horsepower,horse.model4$residuals,xlab="Horsepower",ylab="Residuals")

par(mfrow=c(2,2))
plot(horse.model4)

summary(horse.model4)

#####################################################################################
################# GOLF DATA SET #####################################################
##############Feature Selection and the Bias-Variance Trade Off######################


###setwd("/Users/apurv/Documents/SMU/6372 - Applied Statistics/Unit -2/Live Session/Unit2PreLive")

setwd("/Users/apurv/Documents/SMU/6372 - Applied Statistics/Unit -2/Live Session/Unit2PreLive")
install.packages("leaps")
library(leaps)
golf<-read.csv("GolfData2.csv",header=T)
golf<-golf[,-c(1,8,9,10)]
reg.fwd=regsubsets(log(AvgWinnings)~.,data=golf,method="forward",nvmax=20)

summary(reg.fwd)$adjr2
summary(reg.fwd)$rss
summary(reg.fwd)$bic

### BIC ####

par(mfrow=c(1,3))
bics<-summary(reg.fwd)$bic
plot(1:20,bics,type="l",ylab="BIC",xlab="# of predictors")
index<-which(bics==min(bics))
points(index,bics[index],col="red",pch=10)


#### Adjusted R Square #####

adjr2<-summary(reg.fwd)$adjr2
plot(1:20,adjr2,type="l",ylab="Adjusted R-squared",xlab="# of predictors")
index<-which(adjr2==max(adjr2))
points(index,adjr2[index],col="red",pch=10)

##### RSS #########
rss<-summary(reg.fwd)$rss
plot(1:20,rss,type="l",ylab="train RSS",xlab="# of predictors")
index<-which(rss==min(rss))
points(index,rss[index],col="red",pch=10)


##### Split data set into 2 for test and train #####

set.seed(1234)
index<-sample(1:dim(golf)[1],100,replace=F)
train<-golf[index,]
test<-golf[-index,]
reg.fwd=regsubsets(log(AvgWinnings)~.,data=train,method="forward",nvmax=20)

#Really handy predict function
predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

testASE<-c()
#note my index is to 20 since that what I set it in regsubsets
for (i in 1:20){
  predictions<-predict.regsubsets(object=reg.fwd,newdata=test,id=i) 
  testASE[i]<-mean((log(test$AvgWinnings)-predictions)^2)
}
par(mfrow=c(1,1))
plot(1:20,testASE,type="l",xlab="# of predictors",ylab="test vs train ASE",ylim=c(0.3,0.8))
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
sample <- sample.int(n = nrow(Auto1), size = floor(.50*nrow(Auto1)), replace = F)
train1 <- Auto1[sample, ]
test1  <- Auto1[-sample, ]
summary(test1)
summary(train1)


########### Questions 7.2 ####################
Auto1$cylinders<-as.factor(Auto1$cylinders)
Auto1$origin<-as.factor(Auto1$origin)
par(mfrow=c(1,3))
plot(Auto1$horsepower,log(Auto1$mpg), xlab="horsepower",ylab="Log of mpg")
new<-data.frame(Auto1$horsepower=seq(30,300,.1))
horse.model3<-lm(log(Auto1$mpg)~Auto1$horsepower+I(Auto1$horsepower^2))
lines(seq(30,300,.1),predict(horse.model3,newdata=new),col="red",lwd=4)
plot(horse.model3$fitted.values,horse.model3$residuals,xlab="Fitted Values",ylab="Residuals")
plot(Auto1$horsepower,horse.model3$residuals,xlab="Horsepower",ylab="Residuals")

par(mfrow=c(2,2))
plot(horse.model3)

summary(horse.model3)


### Log MPG and Horsepower ---No Quadratic

par(mfrow=c(1,3))
plot(Auto1$horsepower,log(Auto1$mpg), xlab="horsepower",ylab="Log of mpg")
new<-data.frame(Auto1$horsepower=seq(30,300,.1))
horse.model4<-lm(log(Auto1$mpg)~Auto1$horsepower)
lines(seq(30,300,.1),predict(horse.model4,newdata=new),col="red",lwd=4)
plot(horse.model4$fitted.values,horse.model4$residuals,xlab="Fitted Values",ylab="Residuals")
plot(Auto1$horsepower,horse.model4$residuals,xlab="Horsepower",ylab="Residuals")

par(mfrow=c(2,2))
plot(horse.model4)

summary(horse.model4)



## Scatterplot Horespower vs MPG
plot(Auto1$horsepower,Auto1$mpg, xlab="horsepower",ylab="mpg")
new<-data.frame(Auto1$horsepower=seq(30,300,.1))
lines(seq(30,300,.1),predict(lm(Auto1$mpg~Auto1$horsepower),newdata=new),col="red",lwd=4)

########### Questions 7.3 ####################
Auto2=Auto1[ , -c(2)] ## Remove cylinder row

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






