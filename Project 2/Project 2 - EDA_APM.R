# Install Libraries
library(caret)
library(groupdata2)
library(ggplot2)
library(dplyr)
library(GGally)
library(ggcorrplot)
library(leaps)
library(glmnet)
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
bank.ds$job<-as.factor(bank.ds$job)

bank.ds$emp.var.rate<-as.numeric(bank.ds$emp.var.rate)
bank.ds$cons.price.idx<-as.numeric(bank.ds$cons.price.idx)
bank.ds$cons.conf.idx<-as.numeric(bank.ds$cons.conf.idx)
bank.ds$euribor3m<-as.numeric(bank.ds$euribor3m)
bank.ds$nr.employed <-as.numeric(bank.ds$nr.employed)



bank1 <- bank.ds[,c(1,11,12,21)]
bank2 <- bank.ds[,c(1,11,12, 13,14,16,17,18,19,20)]
bank3 <- bank.ds[,c(1,11,12, 13,14,16,17,18,19,20,21)]

# Splitting into Categorical variables
bank.Fact <- bank.ds[, c(2,3,4,6,7,8,9,10,15,21)]
bank.Fact1 <- bank.ds[, c(2,3,4,6,21)]
bank.Fact2 <- bank.ds[, c(7,8,9,10,15,21)]

# Splitting into Continous variables

bank.Cont <- bank.ds[, c(1,11,12,13,14,16,17,18,19,20)]
bank.Cont1 <- bank.ds[, c(1,11,12,13,14,21)] # Adding Response variable also
bank.Cont2 <- bank.ds[, c(16,17,18,19,20,21)]  # Adding Response variable also

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
# 
# pca1<-prcomp(bank2,scale.=TRUE)
# plot(prcomp(bank2))
# pc.scores<-pca1$x
# pairs(pc.scores)
# cor(pc.scores)
# 
# 
# ######
# 
# 
# prop.table(table(y,month),2)
# plot(y~month,col=c("red","blue"))


### Correlation test for factors and Continous variables


model.matrix(~0+., data=bank.Fact1) %>%
  cor() %>%
  ggcorrplot(show.diag = F, type="upper", lab=TRUE, lab_size=2.5, insig = "blank")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1,size = 8), axis.text.y = element_text(hjust = 1,size = 8))


model.matrix(~0+., data=bank.Fact2) %>%
  cor() %>%
  ggcorrplot(show.diag = F, type="upper", lab=TRUE, lab_size=2.5, insig = "blank")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1,size = 8), axis.text.y = element_text(hjust = 1,size = 8))


model.matrix(~0+., data=bank.Cont1) %>%
  cor() %>%
  ggcorrplot(show.diag = F, type="upper", lab=TRUE, lab_size=3, insig = "blank")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1,size = 10), axis.text.y = element_text(hjust = 1,size = 10))

model.matrix(~0+., data=bank.Cont2) %>%
  cor() %>%
  ggcorrplot(show.diag = F, type="upper", lab=TRUE, lab_size=3, insig = "blank")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1,size = 10), axis.text.y = element_text(hjust = 1,size = 10))



# MODEL

bank.ds.red1 <- subset(bank.ds, select = -c(duration, pdays, default,day_of_week,cons.conf.idx) )

bank.ds.red2 <- subset(bank.ds, select = -c(age,job, marital, education, duration, pdays, default,day_of_week,cons.conf.idx, nr.employed) )


bank.ds.red3 <- subset(bank.ds, select = -c(age,job, marital, loan, housing, previous, education, duration, pdays, default,day_of_week,cons.conf.idx, nr.employed ) )



fit1<-glm(formula = y~., family = binomial(link = "logit"), data = bank.ds.red1)


fit2<-glm(formula = y~., family = binomial(link = "logit"), data = bank.ds.red2)

fit3<-glm(formula = y~., family = binomial(link = "logit"), data = bank.ds.red3)


## Forward selection model

  
# bank.ds.fwd=regsubsets(y~.,data=bank.ds.red1,method="forward",nvmax=21)
# 
# summary(bank.ds.fwd)
# summary(bank.ds.fwd)$adjr2
# summary(bank.ds.fwd)$rss
# summary(bank.ds.fwd)$bic

bank.model= lm(y ~ ., data = bank.ds.red1)
# summary(bank.model)
# formula(bank.model)
# 
# bank.backward = step(bank.model, direction = "backward")

model.first = lm(y ~ 1, data = bank.ds.red1)
bank.ForwdLm = step(model.first, direction = "forward", scope = formula(bank.model))

###==========================================LASSO=============================================###


X = as.matrix(dplyr::select(bank.ds.red1, -c(y)))
Y = as.matrix(dplyr::select(bank.ds.red1, y))
lasso.fit = glmnet(x= X, y = Y, family = 'gaussian', alpha = 1)
plot(lasso.fit, xvar = 'lambda', label = T)
plot(lasso.fit, xvar = 'dev', lable = T)

cv.lasso = cv.glmnet(x=X, y=Y, family = 'gaussian', alpha = 1, nfolds = 10)
plot(cv.lasso)

cv.lasso$lambda.min 
cv.lasso$lambda.1se 


#=== KNN For Marketing Data===#


KNN.Variables <- bank.ds.red1

# In-order to use the factors for KNN Prediction. Convert Factors to Integer

KNN.Variables$job<-as.integer(KNN.Variables$job)
KNN.Variables$marital<-as.integer(KNN.Variables$marital)
KNN.Variables$education<-as.integer(KNN.Variables$education)
KNN.Variables$housing<-as.integer(KNN.Variables$housing)
KNN.Variables$loan<-as.integer(KNN.Variables$loan)
KNN.Variables$contact<-as.integer(KNN.Variables$contact)
KNN.Variables$month<-as.integer(KNN.Variables$month)
KNN.Variables$poutcome<-as.integer(KNN.Variables$poutcome)



# Normalize all the variables

KNN.DF.N <- data.frame(age = scale(KNN.Variables$age), job = scale(KNN.Variables$job),marital = scale(KNN.Variables$marital),
                       education = scale(KNN.Variables$education), housing = scale(KNN.Variables$housing), loan = scale(KNN.Variables$loan),
                       contact = scale(KNN.Variables$contact), month = scale(KNN.Variables$month), campaign = scale(KNN.Variables$campaign),
                       previous = scale(KNN.Variables$previous), poutcome = scale(KNN.Variables$poutcome), emp.var.rate = scale(KNN.Variables$emp.var.rate),
                       cons.price.idx = scale(KNN.Variables$cons.price.idx), euribor3m = scale(KNN.Variables$euribor3m), nr.employed = scale(KNN.Variables$nr.employed), y=KNN.Variables$y)


# Run 10 iterations  on different train/test sets. We will compute the average accuracy, specificity and Sensitivity.
iterations = 10
numks = 50

masterAcc = matrix(nrow = iterations,ncol = numks)
masterSensitivity = matrix(nrow = iterations,ncol = numks)
masterSpecificity = matrix(nrow = iterations,ncol = numks)
splitPerc = .85 #Training / Test split Percentage
for(j in 1:iterations)
{
  splitPerc = .85
  set.seed(12345)
  trainIndices = sample(1:dim(KNN.DF.N)[1],round(splitPerc * dim(KNN.DF.N)[1]))
  train.KNN.DF = KNN.DF.N[trainIndices,]
  test.KNN.DF = KNN.DF.N[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train.KNN.DF[,c(1:15)], test.KNN.DF[,c(1:15)],train.KNN.DF$y,prob=TRUE, k=i)
    table(classifications,test.KNN.DF$y)
    CM = confusionMatrix(table(classifications,test.KNN.DF$y))
    masterAcc[j,i] = CM$overall[1]
    masterSensitivity[j,i] = CM$byClass[1]
    masterSpecificity[j,i] = CM$byClass[2]
  }
  
  MeanAcc = colMeans(masterAcc)
  MeanSensitivity = colMeans(masterSensitivity)
  MeanSpecificity = colMeans(masterSpecificity)
  masterAcc[j] = CM$overall[1]
  masterSensitivity[j] = CM$byClass[1]
  masterSpecificity[j] = CM$byClass[2]
}
plot(seq(1,numks,1),MeanAcc, type = "l", main = "Plot of value of K vs. Accuracy", xlab = "Value of K", ylab="Accuracy %")

#which.max(MeanAcc)
#max(MeanSensitivity)
#max(MeanSpecificity)

# Best K is at 45 with 73.76% Accuracy, Sensitivity is 78.20% and Specificity is 69.33%

MeanAcc = mean(colMeans(masterAcc))
MeanSpecificity = mean(colMeans(masterSpecificity))
MeanSensitivity = mean(colMeans(masterSensitivity))

print(paste("Best Accuracy at K =  ", which.max(colMeans(masterAcc)), "of", max(colMeans(masterAcc))*100,"%"))

print(paste("Mean Accuracy = ", MeanAcc*100,"%"))
print(paste("Mean Sensitivity = ", MeanSensitivity*100,"%"))
print(paste("Mean Specificity = ", MeanSpecificity*100,"%"))




# Run KNN Model for K=41
classifications.K41 = knn(train.KNN.DF[,c(1:15)], test.KNN.DF[,c(1:15)],train.KNN.DF$y,prob=TRUE, k=41)
#table(classifications.K41,test.AT.DF3$Attrition)
CM.K41 = confusionMatrix(table(classifications.K41,test.KNN.DF$y))

print(paste("Accuracy for K-41 = ", CM.K41$overall[1]*100,"%"))
print(paste("Sensitivity for K-41 = ", CM.K41$byClass[1]*100,"%"))
print(paste("Specificity for K-41 = ", CM.K41$byClass[2]*100,"%"))








#=== KNN with ONLY CONTINOUS VARIABLE Marketing Data===#
#======================================================#
#======================================================#


KNN.Variables <- bank.Cont


# Normalize all the variables

KNN.DF.N <- data.frame(age = scale(KNN.Variables$age),campaign = scale(KNN.Variables$campaign), pdays=scale(KNN.Variables$pdays), emp.var.rate = scale(KNN.Variables$emp.var.rate),
                       cons.price.idx = scale(KNN.Variables$cons.price.idx), cons.conf.idx = scale(KNN.Variables$cons.conf.idx), euribor3m = scale(KNN.Variables$euribor3m), 
                       nr.employed = scale(KNN.Variables$nr.employed), y=bank.ds$y)


# Run 10 iterations  on different train/test sets. We will compute the average accuracy, specificity and Sensitivity.
iterations = 10
numks = 50

masterAcc = matrix(nrow = iterations,ncol = numks)
masterSensitivity = matrix(nrow = iterations,ncol = numks)
masterSpecificity = matrix(nrow = iterations,ncol = numks)
splitPerc = .85 #Training / Test split Percentage
for(j in 1:iterations)
{
  splitPerc = .85
  set.seed(12345)
  trainIndices = sample(1:dim(KNN.DF.N)[1],round(splitPerc * dim(KNN.DF.N)[1]))
  train.KNN.DF = KNN.DF.N[trainIndices,]
  test.KNN.DF = KNN.DF.N[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train.KNN.DF[,c(1:8)], test.KNN.DF[,c(1:8)],train.KNN.DF$y,prob=TRUE, k=i)
    table(classifications,test.KNN.DF$y)
    CM = confusionMatrix(table(classifications,test.KNN.DF$y))
    masterAcc[j,i] = CM$overall[1]
    masterSensitivity[j,i] = CM$byClass[1]
    masterSpecificity[j,i] = CM$byClass[2]
  }
  
  MeanAcc = colMeans(masterAcc)
  MeanSensitivity = colMeans(masterSensitivity)
  MeanSpecificity = colMeans(masterSpecificity)
  masterAcc[j] = CM$overall[1]
  masterSensitivity[j] = CM$byClass[1]
  masterSpecificity[j] = CM$byClass[2]
}
plot(seq(1,numks,1),MeanAcc, type = "l", main = "Plot of value of K vs. Accuracy", xlab = "Value of K", ylab="Accuracy %")

#which.max(MeanAcc)
#max(MeanSensitivity)
#max(MeanSpecificity)

# Best K is at 20 with 73.83% Accuracy, Sensitivity is 78.72% and Specificity is 69.19%

MeanAcc = mean(colMeans(masterAcc))
MeanSpecificity = mean(colMeans(masterSpecificity))
MeanSensitivity = mean(colMeans(masterSensitivity))

print(paste("Best Accuracy at K =  ", which.max(colMeans(masterAcc)), "of", max(colMeans(masterAcc))*100,"%"))

print(paste("Mean Accuracy = ", MeanAcc*100,"%"))
print(paste("Mean Sensitivity = ", MeanSensitivity*100,"%"))
print(paste("Mean Specificity = ", MeanSpecificity*100,"%"))




# Run KNN Model for K=20
classifications.K20 = knn(train.KNN.DF[,c(1:15)], test.KNN.DF[,c(1:15)],train.KNN.DF$y,prob=TRUE, k=20)
#table(classifications.K41,test.AT.DF3$Attrition)
CM.K20 = confusionMatrix(table(classifications.K20,test.KNN.DF$y))

print(paste("Accuracy for K-20 = ", CM.K41$overall[1]*100,"%"))
print(paste("Sensitivity for K-20 = ", CM.K41$byClass[1]*100,"%"))
print(paste("Specificity for K-20 = ", CM.K41$byClass[2]*100,"%"))


