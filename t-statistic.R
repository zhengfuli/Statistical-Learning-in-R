set.seed(1)
x = rnorm(100)
y = 2 * x + rnorm(100)
lm.fit = lm(y ~ x + 0)

par(mfrow=c(1,2))
plot(x, y)
abline(lm.fit)

lm.fit = lm(x ~ y + 0)
plot(y, x)
abline(lm.fit)
print(summary(lm.fit))

print((sqrt(length(x)-1) * sum(x*y)) / (sqrt(sum(x*x) * sum(y*y) - (sum(x*y))^2)))

lm.fit1 = lm(x ~ y)
lm.fit2 = lm(y ~ x)
print(summary(lm.fit1))
print(summary(lm.fit2))
