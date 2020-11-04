######## KNN ###########
library(caret)
life = read.csv("/Users/apurv/Documents/SMU/6372 - Applied Statistics/Projects/Project 1/Life Expectancy Data.csv", header = TRUE)

#######################################################
##################Sample - Method 1####################
#######################################################
life1=na.omit(life) # remove all NA - just for running the model. This is NOT to be used in final model.

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




#######################################################
##################Sample - Method 2####################
#######################################################
life1$LOGunder.five.deaths = log(life1$under.five.deaths)
life1$LOGHIV = log(life1$HIV.AIDS)
life1$LOGPercExp = log(life1$percentage.expenditure)
life1$Status=as.factor(life1$Status)

set.seed(123)
training.samples <- life1$Life.expectancy %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- life1[training.samples, ]
test.data <- life1[-training.samples, ]

# Fit the model on the training set
set.seed(123)
model <- train(
  Life.expectancy~Status+Adult.Mortality+Alcohol+percentage.expenditure+Measles+BMI+under.five.deaths+Polio+Total.expenditure+Diphtheria+HIV.AIDS+thinness..1.19.years+Income.composition.of.resources+Schooling, data = train.data, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 10
)
# Plot model error RMSE vs different values of k
plot(model)
# Best tuning parameter k that minimize the RMSE
model$bestTune
# Make predictions on the test data
predictions <- model %>% predict(test.data)
head(predictions)
# Compute the prediction error RMSE
RMSE(predictions, test.data$Life.expectancy)