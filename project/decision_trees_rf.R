names(iris)
set.seed(1)

library(party)
library(randomForest)

samples = sample(1:nrow(iris))
ct_cv_error = rep(0, 10)
rf_cv_error = rep(0, 10)

for(i in 1:10)
{
    fold = ((i-1)*length(samples)/10+1):(i*length(samples)/10)
    valid = iris[samples[fold],]
    train = iris[samples[-fold],]

    ct = ctree(Species ~ ., data = train)
    rf = randomForest(Species ~ ., data = train, mtry = 2, ntree = 100)

    pred =  predict(ct, newdata = valid)
    ct_cv_error[i] = mean(pred != valid$Species)

    pred =  predict(rf, newdata = valid)
    rf_cv_error[i] = mean(pred != valid$Species)
}

print(mean(ct_cv_error))
classification_tree = ctree(Species ~ ., data = iris)
# print(classification_tree)
plot(classification_tree, type = "simple")

print(mean(rf_cv_error))
random_forest = randomForest(Species ~ ., data = iris, mtry = 2, ntree = 100)

print(importance(random_forest))
plot(margin(random_forest, iris$Species), xlab = "Observations", ylab = "Probability of correct prediction")
#