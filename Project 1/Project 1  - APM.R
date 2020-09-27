life = read.csv("/Users/apurv/Documents/SMU/6372 - Applied Statistics/Projects/Project 1/Life Expectancy Data.csv", header = TRUE)

life1=na.omit(life)
pairs(life1)
######## KNN ###########
library(caret)
set.seed(123) # set seed data
iterations = 100
numks = 50
splitPerc = .70

masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  trainIndices = sample(1:dim(life)[1],round(splitPerc * dim(life)[1]))
  train = life[trainIndices,]
  test = life[-trainIndices,]
  train1=train%>%filter(!is.na(Adult.Mortality)&!is.na(Alcohol)&!is.na(percentage.expenditure)&!is.na(GDP)&!is.na(Schooling)&!is.na(Life.expectancy))
  test1=test%>%filter(!is.na(Adult.Mortality)&!is.na(Alcohol)&!is.na(percentage.expenditure)&!is.na(GDP)&!is.na(Schooling)&!is.na(Life.expectancy))
  
  for(i in 1:numks)
  {
    classifications = knn(train1[,c(5,7,8,17,22)],test1[,c(5,7,8,17,22)],train1$Life.expectancy, prob = TRUE, k = i)
    table(classifications,test1$Life.expectancy)
    CM = confusionMatrix(table(classifications,test1$Life.expectancy))
    masterAcc[j,i] = CM$overall[1]
  }
  
}

MeanAcc = colMeans(masterAcc)

plot(seq(1,numks,1),MeanAcc, type = "l", main = "Plot of value of K vs. Accuracy", xlab = "Value of K", ylab="Accuracy %")

which.max(MeanAcc)
max(MeanAcc)




