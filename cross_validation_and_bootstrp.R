# n = 1:1e+05;Probs = 1-(1-1/n)^n;plot(n, Probs);lines(n, Probs)

# store=rep(NA , 10000)
# for (i in 1:10000)
# {
#     store[i]=sum(sample (1:100 , rep =TRUE)==4) >0
# }
# print(mean(store))

library(ISLR)
library(ggplot2)
#
# # set.seed(1)
# attach(Default)
# fit = glm(default ~ income + balance, family = binomial)
#
# print(coef(fit))
# intercept = coef(fit)[1]/(-coef(fit)[2])
# slope = coef(fit)[3]/(-coef(fit)[2])
#
# p = ggplot(Default, aes(x = balance, y = income, colour = default, shape = default)) +
#     geom_point() + stat_function(fun=function(x){intercept+slope*x}, colour = "black") +
#     coord_cartesian(ylim=c(0, 1e5))
# plot(p)

test_error = function(n)
{
   error = rep(0, 2*n)
   for (i in 1:n)
   {
       train = sample(1:nrow(Default), nrow(Default)*4/5)
       glm.fit1 = glm(default ~ income + balance, data = Default[train,], family = binomial)
       glm.fit2 = glm(default ~ income + balance + student, data = Default[train,], family = binomial)

       glm.pred1 = rep("No", nrow(Default)/5)
       glm.probs1 = predict(glm.fit1, Default[-train,], type = "response")
       glm.pred1[glm.probs1 > 0.5] = "Yes"
       error[i] = mean(glm.pred1 != Default[-train,]$default)

       glm.pred2 = rep("No", nrow(Default)/5)
       glm.probs2 = predict(glm.fit2, Default[-train,], type = "response")
       glm.pred2[glm.probs2 > 0.5] = "Yes"
       error[i+n] = mean(glm.pred2 != Default[-train,]$default)
   }
   return(error)
}

res = test_error(2000)
Error = data.frame(group = factor(rep(c("Without Student","With Student"), each = 2000)), error = res)
library(plyr)
Error_by_group = ddply(Error, "group", summarise, error.mean=mean(error))

p = ggplot(Error, aes(x = error, fill = group)) +
    geom_histogram(bins = 30, alpha = .5, position = "identity") +
    geom_vline(data = Error_by_group, aes(xintercept=error.mean,  colour=group),
               linetype="dashed", size=1)
plot(p)

# p = ggplot(Error, aes(x)) + geom_histogram(aes(y=..density..), bins=40, colour="black", fill="blue") +
#     geom_density(alpha=.2, fill="#FF6666") +
#     geom_vline(aes(xintercept=mean(x, na.rm=T)), color="red", linetype="dashed", size=1)
