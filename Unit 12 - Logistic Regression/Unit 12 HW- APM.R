library(ISLR)
newAuto<-Auto
#Creating a categorical response for illustrative purposes.
newAuto$mpg<-factor(ifelse(Auto$mpg>median(Auto$mpg),"High","Low"),levels=c("Low","High"))  #last level is the success

newAuto$cylinders<-factor(newAuto$cylinders)
#Removing data with 3 and 5 cylinders and making sure are R treats the predictor with just 3 levels and not 5.
newAuto<-newAuto[-which(newAuto$cylinders %in% c(3,5)),]
newAuto$cylinders<-factor(newAuto$cylinders)
#Creating just two origins by combining 2 and 3 again for keeping things simple.
newAuto$origin[which(newAuto$origin==3)]<-2
newAuto$origin<-factor(newAuto$origin)

#From here we are going to do a simple split of the data set and explore the training
#data set newAuto.  The test will be held out for assessment of the model fits.
set.seed(1234)
index<-sample(1:385,250,replace=FALSE)
test<-newAuto[-index,-9]
train<-newAuto[index,-9]

library(GGally)
ggpairs(newAuto,columns=2:8,aes(colour=mpg))

attach(newAuto)
prop.table(table(mpg,cylinders),2)
plot(mpg~cylinders,col=c("red","blue"))

t(aggregate(weight~mpg,data=newAuto,summary))
plot(weight~mpg,col=c("red","blue"))

############# QUESTION 5###########
#############

prop.table(table(mpg,origin),2)
plot(mpg~origin,col=c("green","orange"))

t(aggregate(horsepower~mpg,data=newAuto,summary))
plot(horsepower~mpg,col=c("green","steelblue"))

t(aggregate(year~mpg,data=newAuto,summary))
plot(year~mpg,col=c("green","steelblue"))

t(aggregate(acceleration~mpg,data=newAuto,summary))
plot(acceleration~mpg,col=c("green","steelblue"))

t(aggregate(displacement~mpg,data=newAuto,summary))
plot(displacement~mpg,col=c("green","steelblue"))



############# QUESTION 6###########
#############

plot(weight~ cylinders,col=c("green","steelblue"))
plot(origin~cylinders,col=c("green","steelblue"))
