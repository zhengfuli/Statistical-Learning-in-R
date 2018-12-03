names(iris)
library(ggplot2)
library(GGally)
library(caret)
library(nnet)
attach(iris)

# multinomial Logistic regression
set.seed(1)

samples = sample(1:nrow(iris))
cv_error = rep(0, 10)

for(i in 1:10)
{
    fold = ((i-1)*length(samples)/10+1):(i*length(samples)/10)
    valid = iris[samples[fold],]
    train = iris[samples[-fold],]

    mlr = multinom(Species ~ ., data = train)
    # print(summary(mlr))

    probs = predict(mlr, valid, "probs")
    cum_probs = t(apply(probs, 1, cumsum))

    # Draw random values
    vals = runif(nrow(valid))

    # Join cumulative probabilities and random draws
    tmp = cbind(cum_probs, vals)

    # For each row, get choice index.
    k = ncol(probs)
    ids = 1 + apply(tmp, 1, function(x){length(which(x[1:k] < x[k+1]))})

    # print(ids)
    cv_error[i] = mean(ids != as.numeric(valid$Species))
}
print(mean(cv_error))