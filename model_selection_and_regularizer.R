set.seed(1)
X = rnorm(100)
e = rnorm(100)

Y = 4 - 3 * X + 2 * X^2 - X^3 + e

library(leaps)
data = data.frame(y = Y, x = X)
models = regsubsets(y ~ poly(x, 10, raw = T), data = data, nvmax = 10)
summary = summary(models)

print(which.min(summary$cp))
print(which.min(summary$bic))
print(which.max(summary$adjr2))

plot(summary$cp, xlab = "Number of predictors", ylab = "Cp", pch = 20, type = "l")
points(4, summary$cp[4], pch = 4, col = "red", lwd = 7)