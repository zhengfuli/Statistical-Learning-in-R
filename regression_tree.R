library(ISLR)
attach(Carseats)

set.seed(3)
train = sample(dim(Carseats)[1], dim(Carseats)[1]*3/4)
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[-train, ]

library(rpart)
library(rpart.plot)
fit = rpart(Sales ~ ., data = Carseats.train)
summary(fit)
# rpart.plot(fit)

pred = predict(fit, Carseats.test)
print(mean((Carseats.test$Sales - pred)^2))

# printcp(fit)
# plotcp(fit)

# fit = prune(fit, cp = 0.0349)
# summary(fit)
# rpart.plot(fit)
#
# pred = predict(fit, Carseats.test)
# print(mean((Carseats.test$Sales - pred)^2))
library(ggplot2)
library(randomForest)
# bagging = randomForest(Sales ~ ., data = Carseats.train, mtry = 10, ntree = 500, importance = T)
# pred = predict(bagging, Carseats.test)
# print(mean((Carseats.test$Sales - pred)^2))
# print(importance(bagging))

MSE = rep(0, 10)
m = seq(1, 10, 1)
for(i in m)
{
    rf = randomForest(Sales ~ ., data = Carseats.train, mtry = i, ntree = 500, importance = T)
    pred = predict(rf, Carseats.test)
    MSE[i] = mean((Carseats.test$Sales - pred)^2)
}
plot(ggplot() + geom_line(data = data.frame(m, MSE), aes(m, MSE), col = "#D55E00") +
geom_point(data = data.frame(m, MSE), aes(m, MSE), col = "#0072B2"))
