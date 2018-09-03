set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100) / 10
y = 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

print(cor(x1, x2))
# plot(x1, x2)

lm.fit = lm(y ~ x1 + x2)
print(lm.fit$coefficients)

lm.fit1 = lm(y ~ x1)
print(summary(lm.fit1))

lm.fit2 = lm(y ~ x2)
print(summary(lm.fit2))

x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)

lm.fit3 = lm(y ~ x1 + x2)
print(summary(lm.fit3))
# par(mfrow=c(2,2))
# plot(lm.fit3)

# library(rgl)
# plot3d(x1, x2, y, size = 10)
# fix(y)

# lm.fit4 = lm(y ~ x1)
# print(summary(lm.fit4))
# par(mfrow=c(2,2))
# plot(lm.fit4)
# plot(x1, y)
# abline(lm.fit1, col="blue")
# abline(lm.fit4, col="red")

lm.fit5 = lm(y ~ x2)
print(summary(lm.fit5))
par(mfrow=c(2,2))
plot(lm.fit5)
# plot(x2, y)
# abline(lm.fit2, col="blue")
# abline(lm.fit5, col="red")

