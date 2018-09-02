auto = read.csv("./data/Auto.csv", na.strings="?")
auto = na.omit(auto)
print(summary(auto))
attach(auto)
lm.model = lm(mpg ~ horsepower)
print(summary(lm.model))

print(predict(lm.model, data.frame(horsepower = c(98)), interval="confidence"))
print(predict(lm.model, data.frame(horsepower = c(98)), interval="prediction"))

# plot(horsepower, mpg)
# abline(lm.model)

par(mfrow=c(2,2))
plot(lm.model)
