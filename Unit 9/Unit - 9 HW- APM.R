library(Lahman)
data(Batting)
index<-which(Batting$yearID==2016)
Bat16<-Batting[index,]
summary(Bat16)

reduced<-Bat16[,6:22]
pairs(reduced)

apply(reduced,2,summary)
var.raw<-apply(reduced,2,var)
var.raw
#Total variance
sum(var.raw)

cov(reduced)
#Another way to get total variance
sum(diag(cov(reduced)))

pc.result<-prcomp(reduced,scale.=TRUE)
pc.scores<-pc.result$x
pairs(pc.scores)
cor(pc.scores)

var.pca<-apply(pc.scores,2,var)
var.pca
#Total Variance of PC's
sum(var.pca)
#Total Variance of Original Variables.
sum(var.raw)

#List of eigenvectors
pc.result$rotation

par(mfrow=c(1,2))
eigenvals<-(pc.result$sdev)^2
plot(1:17,eigenvals/sum(eigenvals),type="l",main="Scree Plot",ylab="Prop. Var. Explained")
cumulative.prop<-cumsum(eigenvals/sum(eigenvals))
plot(1:17,cumulative.prop,type="l",main="Cumulative proportion",ylim=c(0,1))
par(mfrow=c(1,1))

## HW Assignment #2
## 

bc<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",header=F,sep=",")
names(bc)<- c('id_number', 'diagnosis', 'radius_mean', 
              'texture_mean', 'perimeter_mean', 'area_mean', 
              'smoothness_mean', 'compactness_mean', 
              'concavity_mean','concave_points_mean', 
              'symmetry_mean', 'fractal_dimension_mean',
              'radius_se', 'texture_se', 'perimeter_se', 
              'area_se', 'smoothness_se', 'compactness_se', 
              'concavity_se', 'concave_points_se', 
              'symmetry_se', 'fractal_dimension_se', 
              'radius_worst', 'texture_worst', 
              'perimeter_worst', 'area_worst', 
              'smoothness_worst', 'compactness_worst', 
              'concavity_worst', 'concave_points_worst', 
              'symmetry_worst', 'fractal_dimension_worst')

#Getting a look at the distribution
table(bc$diagnosis)

bc$diagnosis<-as.factor(bc$diagnosis)
#Scatter plots color coded by response for just the first few variables
pairs(bc[,3:6],col=bc$diagnosis)

pc.bc<-prcomp(bc[,-c(1,2)],scale.=TRUE)
pc.bc.scores<-pc.bc$x

#Adding the response column to the PC's data frame
pc.bc.scores<-data.frame(pc.bc.scores)
pc.bc.scores$Diagnosis<-bc$diagnosis

#Use ggplot2 to plot the first few pc's
library(ggplot2)
ggplot(data = pc.bc.scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col=Diagnosis), size=1)+
  ggtitle("PCA of Breast Cancer Tumor Biopsies")

ggplot(data = pc.bc.scores, aes(x = PC2, y = PC3)) +
  geom_point(aes(col=Diagnosis), size=1)+
  ggtitle("PCA of Breast Cancer Tumor Biopsies")









mylda<- lda(diagnosis ~ radius_mean+
            texture_mean+ perimeter_mean+ area_mean, data = bc)






#confusion matrix
prd<-predict(mylda, newdata = bc)$class
table(prd,bc$diagnosis)


#Plot
#
# draw discrimination line
np <- 300
nd.x <- seq(from = min(full$X1), to = max(full$X1), length.out = np)
nd.y <- seq(from = min(full$X2), to = max(full$X2), length.out = np)
nd <- expand.grid(X1 = nd.x, X2 = nd.y)

prd <- as.numeric(predict(mylda, newdata = nd)$class)

plot(full[, 1:2], col = full$Response, main="Shift in X2")
points(mylda$means, pch = "+", cex = 2, col = c("black", "red"))
contour(x = nd.x, y = nd.y, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2), add = TRUE, drawlabels = FALSE)



######## 2
######## 
fake<-bc
fake$diagnosis<-sample(fake$diagnosis,569,replace=F)


pc.fake<-prcomp(fake[,-c(1,2)],scale.=TRUE)
pc.fake.scores<-pc.fake$x

#Adding the response column to the PC's data frame
pc.fake.scores<-data.frame(pc.fake.scores)
pc.fake.scores$Diagnosis<-fake$diagnosis

#Use ggplot2 to plot the first few pc's
library(ggplot2)
ggplot(data = pc.fake.scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col=Diagnosis), size=1)+
  ggtitle("PCA of Breast Cancer Tumor Biopsies")



####################3
####################



mylda1<- lda(diagnosis ~ radius_mean+
              texture_mean+ perimeter_mean+ area_mean, data = fake)






#confusion matrix
prd1<-predict(mylda1, newdata = fake)$class
table(prd1,fake$diagnosis)