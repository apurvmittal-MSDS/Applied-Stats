#Omit.na
lifeexpectency.nona = na.omit(lifeexpectency)

#looks normal, but lot of missing data, so we are dropping it
#alternatively can pick from other sites, which Dr. Tuner said, not really needed
hist(lifeexpectency$GDP)

#thinness.5.9 requires log
hist(lifeexpectency$thinness.5.9.years)
qqplot(log(lifeexpectency$thinness.5.9.years), lifeexpectency$Life.expectancy)
hist(log(lifeexpectency$thinness.5.9.years))

#infant.deaths requires log transform
qqplot(lifeexpectency$infant.deaths, lifeexpectency$Life.expectancy)
qqplot(log10(lifeexpectency$infant.deaths), lifeexpectency$Life.expectancy)
hist(log10(lifeexpectency$infant.deaths))

#Alcohol is rightskewed, but none of the transformation makes it normal
#so we may have to drop this
qqplot(log(lifeexpectency$Alcohol), lifeexpectency$Life.expectancy)
qqplot(lifeexpectency$Alcohol, lifeexpectency$Life.expectancy)

#Cannot transform. cant use as variable
qqplot(lifeexpectency$HIV.AIDS, lifeexpectency$Life.expectancy)
qqplot(log(lifeexpectency$HIV.AIDS), lifeexpectency$Life.expectancy)

#Right skewed, use log 
qqplot(lifeexpectency$Income.composition.of.resources, lifeexpectency$Life.expectancy)
qqplot(log(lifeexpectency$Income.composition.of.resources), lifeexpectency$Life.expectancy)
hist(log(lifeexpectency$Income.composition.of.resources))

#Right skewed, use log (from qqplot, histogram showed normal)
qqplot(log(lifeexpectency$Schooling), lifeexpectency$Life.expectancy)
qqplot(lifeexpectency$Schooling, lifeexpectency$Life.expectancy)

#Right skewed, log looks good
qqplot(lifeexpectency$Total.expenditure, lifeexpectency$Life.expectancy)
qqplot(log(lifeexpectency$Total.expenditure), lifeexpectency$Life.expectancy)
hist(log(lifeexpectency$Total.expenditure))

#Right skewed, log looks good
qqplot(log(lifeexpectency$percentage.expenditure), lifeexpectency$Life.expectancy)
qqplot((lifeexpectency$percentage.expenditure), lifeexpectency$Life.expectancy)
qqplot(log(lifeexpectency$percentage.expenditure), lifeexpectency$Life.expectancy)
hist(log(lifeexpectency$percentage.expenditure))

#Moderately right skewed, sqrt is recomendded for moderately right skewed
hist(lifeexpectency$Adult.Mortality)
qqplot(lifeexpectency$Adult.Mortality, lifeexpectency$Life.expectancy)
hist(sqrt(lifeexpectency$Adult.Mortality))
qqplot(sqrt(lifeexpectency$Adult.Mortality), lifeexpectency$Life.expectancy)

#Hist is skewed on both, transform to sqrt
hist(lifeexpectency$Total.expenditure)
hist(sqrt(lifeexpectency$Total.expenditure))
qqnorm(sqrt(lifeexpectency$Total.expenditure))


#Transform the above mentioned data in to data.frame
lifeexpectency.nona$l.thinness.5.9.years = log(lifeexpectency.nona$thinness.5.9.years)
lifeexpectency.nona$l.thinness.5.9.years = ifelse(is.infinite(lifeexpectency.nona$l.thinness.5.9.years) ,0,lifeexpectency.nona$l.thinness.5.9.years)

#log10 seems to be better fit than log normal
lifeexpectency.nona$l.infant.deaths=log10(lifeexpectency.nona$infant.deaths)
lifeexpectency.nona$l.infant.deaths= ifelse(is.infinite(lifeexpectency.nona$l.infant.deaths) ,0,lifeexpectency.nona$l.infant.deaths)

lifeexpectency.nona$l.Income.composition.of.resources = log(lifeexpectency.nona$Income.composition.of.resources)
lifeexpectency.nona$l.Income.composition.of.resources = ifelse(is.infinite(lifeexpectency.nona$l.Income.composition.of.resources ) ,0,lifeexpectency.nona$l.Income.composition.of.resources) 

lifeexpectency.nona$l.Schooling= log(lifeexpectency.nona$Schooling)
lifeexpectency.nona$l.Schooling= ifelse(is.infinite(lifeexpectency.nona$l.Schooling) ,0,lifeexpectency.nona$l.Schooling)

lifeexpectency.nona$l.Total.expenditure = log(lifeexpectency.nona$Total.expenditure)
lifeexpectency.nona$l.Total.expenditure= ifelse(is.infinite(lifeexpectency.nona$l.Total.expenditure) ,0,lifeexpectency.nona$l.Total.expenditure)

lifeexpectency.nona$l.percentage.expenditure = log(lifeexpectency.nona$percentage.expenditure)
lifeexpectency.nona$l.percentage.expenditure= ifelse(is.infinite(lifeexpectency.nona$l.percentage.expenditure) ,0,lifeexpectency.nona$l.percentage.expenditure)

lifeexpectency.nona$sqrt.Adult.Mortality = sqrt(lifeexpectency.nona$Adult.Mortality)

lifeexpectency.nona$sqrt.Total.expenditure = sqrt(lifeexpectency.nona$Total.expenditure)

#get fit

fit = lm(formula =Life.expectancy ~ l.infant.deaths  + l.thinness.5.9.years + l.Income.composition.of.resources + l.Schooling+ Status+ l.percentage.expenditure + sqrt.Adult.Mortality + sqrt.Total.expenditure, data = lifeexpectency)
step(fit)


lm(formula = Life.expectancy ~ l.thinness.5.9.years + l.Income.composition.of.resources + 
    l.Schooling + Status + l.percentage.expenditure + sqrt.Adult.Mortality + 
    sqrt.Total.expenditure, data = lifeexpectency.nona)

#total expenditure is above alpha
#so removing total expenditure

fit = lm(formula = Life.expectancy ~ l.thinness.5.9.years + l.Income.composition.of.resources + 
    l.Schooling + Status + l.percentage.expenditure + sqrt.Adult.Mortality, 
    data = lifeexpectency.nona)

#kNN
le_knn3 = train(Life.expectancy ~ l.thinness.5.9.years + l.Income.composition.of.resources + l.Schooling + Status + l.percentage.expenditure + sqrt.Adult.Mortality, data = lifeexpectency.nona, method= "knn", trControl=trainControl(method = 'cv', number = 3, search="grid"), tuneGrid = grid)
le_knn4 = train(Life.expectancy ~ l.thinness.5.9.years + l.Income.composition.of.resources + l.Schooling + Status + l.percentage.expenditure + sqrt.Adult.Mortality, data = lifeexpectency.nona, method= "knn", trControl=trainControl(method = 'cv', number = 4, search="grid"), tuneGrid = grid)
le_knn5 = train(Life.expectancy ~ l.thinness.5.9.years + l.Income.composition.of.resources + l.Schooling + Status + l.percentage.expenditure + sqrt.Adult.Mortality, data = lifeexpectency.nona, method= "knn", trControl=trainControl(method = 'cv', number = 5, search="grid"), tuneGrid = grid)

#Plots
#5
plot(le_knn5$results$k,le_knn5$results$RMSE, col="blue", type="l")
points(le_knn5$results$k[which.min(le_knn5$results$RMSE)],min(le_knn5$results$RMSE), col="red")
#4
lines(le_knn4$results$k,le_knn4$results$RMSE, col="red", type="l")
points(le_knn4$results$k[which.min(le_knn4$results$RMSE)],min(le_knn4$results$RMSE), col="red")
#3
lines(le_knn3$results$k,le_knn3$results$RMSE, col="purple", type="l")
points(le_knn3$results$k[which.min(le_knn3$results$RMSE)]+1,min(le_knn3$results$RMSE), col="purple")
