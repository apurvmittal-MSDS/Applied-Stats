# Install Libraries
library(caret)
library(groupdata2)
library(ggplot2)
library(dplyr)
library(GGally)
# Read the Data
bank_Data <- read.csv2("/Users/apurv/Documents/SMU/6372 - Applied Statistics/R-Code/Applied-Stats/Project 2/bank-additional-full.csv", header = TRUE, sep = ";")

# Convert response variable to a factor
bank_Data$y<-as.factor(bank_Data$y)

# Check summary of original data
summary(bank_Data)

# Several "Unknown" or missing data. Assigning all unknown as NA 
bank_Data[bank_Data=="unknown"] <- NA
sum(is.na(bank_Data))

# Removing all the NAs
bank.nona = na.omit(bank_Data)
sum(is.na(bank.nona))

# Since the data is imbalance and the number of "Yes" for the response variable are very small compared to "No". Downsampling the data to make it balanced.
set.seed(12345)
bank.ds <-downsample(bank.nona, cat_col = "y")
summary(bank.ds)


# Write the downsampled data to a .csv for reference

#write.csv(bank.ds,file="/Users/apurv/Documents/SMU/6372 - Applied Statistics/R-Code/Applied-Stats/Project 2/downsampled.csv")

#########____________________________##############
#########___________EDA______________##############
#########____________________________##############

attach(bank.ds)


bank.ds$month<-as.factor(bank.ds$month)
bank.ds$marital<-as.factor(bank.ds$marital)
bank.ds$education<-as.factor(bank.ds$education)
bank.ds$default<-as.factor(bank.ds$default)
bank.ds$housing<-as.factor(bank.ds$housing)
bank.ds$loan<-as.factor(bank.ds$loan)
bank.ds$contact<-as.factor(bank.ds$contact)
bank.ds$day_of_week<-as.factor(bank.ds$day_of_week)
bank.ds$poutcome<-as.factor(bank.ds$poutcome)

bank.ds$emp.var.rate<-as.numeric(bank.ds$emp.var.rate)
bank.ds$cons.price.idx<-as.numeric(bank.ds$cons.price.idx)
bank.ds$cons.conf.idx<-as.numeric(bank.ds$cons.conf.idx)
bank.ds$euribor3m<-as.numeric(bank.ds$euribor3m)
bank.ds$nr.employed <-as.numeric(bank.ds$nr.employed)



bank1 <- bank.ds[,c(1,11,12,21)]
bank2 <- bank.ds[,c(1,11,12, 13,14,16,17,18,19,20)]
bank3 <- bank.ds[,c(1,11,12, 13,14,16,17,18,19,20,21)]

# Categorical

bank_cat1 = bank.ds[,2:10]

pairs(bank1)
aggregate(y~age,data=bank.ds,summary)
plot(age~y,col=c("red","blue"))

plot(campaign~y, col=c("red","blue"))

plot(duration~y, col=c("red","blue"))

plot(emp.var.rate~y, col=c("red","blue"))

plot(cons.conf.idx~y, col=c("red","blue"))

ggplot(bank.ds,aes(x = month, fill = y)) + geom_bar(position = "dodge")

ggplot(bank.ds,aes(x = marital, fill = y)) + geom_bar(position = "dodge")

ggplot(bank.ds,aes(x = education, fill = y)) + geom_bar(position = "dodge")

ggplot(bank.ds,aes(x = default, fill = y)) + geom_bar(position = "dodge")

ggplot(bank.ds,aes(x = poutcome, fill = y)) + geom_bar(position = "dodge")

ggplot(bank.ds,aes(x = day_of_week, fill = y)) + geom_bar(position = "dodge")

ggplot(bank.ds, aes(x = y, fill = marital)) + geom_density() + labs(title = " Distribution by Marital Status")
#
## All Default values are NO

summary(bank.ds$default)


## Aggregates
aggregate(y~marital,data=bank.ds,summary)

aggregate(y~month,data=bank.ds,summary)

aggregate(y~default,data=bank.ds,summary)

aggregate(y~education,data=bank.ds,summary)
  
aggregate(y~poutcome,data=bank.ds,summary)

## GGpairs

ggpairs(bank1,aes(colour=y))

ggpairs(bank2,aes(colour=y))

ggpairs(bank_cat1,aes(colour=y))

####### PCA ##########

pca1<-prcomp(bank2,scale.=TRUE)
plot(prcomp(bank2))
pc.scores<-pca1$x
pairs(pc.scores)
cor(pc.scores)


######


prop.table(table(y,month),2)
plot(y~month,col=c("red","blue"))