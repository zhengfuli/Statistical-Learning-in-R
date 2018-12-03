names(iris)
library(MASS)
set.seed(1)
iris$Species = factor(iris$Species, labels = c(1,2,3))
# fix(iris)
samples = sample(1:nrow(iris))
lda_cv_error = rep(0, 10)
qda_cv_error = rep(0, 10)

for(i in 1:10)
{
    fold = ((i-1)*length(samples)/10+1):(i*length(samples)/10)
    valid = iris[samples[fold],]
    train = iris[samples[-fold],]

    lda = lda(Species ~ ., data = train)
    qda = qda(Species ~ ., data = train)

    pred = predict(lda, newdata = valid)
    lda_cv_error[i] = mean(pred$class != valid$Species)

    pred = predict(qda, newdata = valid)
    qda_cv_error[i] = mean(pred$class != valid$Species)
}
print(mean(lda_cv_error))
print(mean(qda_cv_error))