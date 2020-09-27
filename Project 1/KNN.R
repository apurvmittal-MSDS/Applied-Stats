######## KNN ###########
library(caret)
life = read.csv("/Users/apurv/Documents/SMU/6372 - Applied Statistics/Projects/Project 1/Life Expectancy Data.csv", header = TRUE)

life1=na.omit(life)
names(life1)
# standard.life1=scale(life1[,c(-1,-2,-3,-4)])
# splitPerc = .70
# 
# set.seed(1)
# trainIndices = sample(1:dim(life1)[1],round(splitPerc * dim(life1)[1]))
# train.life1 = standard.life1[trainIndices,]
# test.life1 = standard.life1[-trainIndices,]
# train.lifeexp = life1$Life.expectancy[trainIndices]
# test.lifeexp = life1$Life.expectancy[-trainIndices]
# 
# 
# life.knn = knn.cv(train.life1,test.life1,train.lifeexp, prob = TRUE, k = 3)
# table(life.knn,test.lifeexp)


test =1:500
standardized.X=scale(life1[,c(-1,-2,-3,-4)])
train.X=standardized.X[-test ,]
test.X=standardized.X[test ,]
train.Y=life1$Life.expectancy[-test]
test.Y=life1$Life.expectancy[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=100)
sum(test.Y!=knn.pred)
table(knn.pred,test.Y)
