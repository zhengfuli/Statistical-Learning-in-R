library(ggplot2)

p = seq(0, 1, 0.01)
gini = 2* p * (1 - p)
cross_entropy = -(p * log(p) + (1 - p) * log(1 - p))
classification_error = 1 - pmax(p, 1 - p)

# plot(ggplot() + geom_line(data = data.frame(p, gini), aes(p, gini), col = "#0072B2") +
# geom_line(data = data.frame(p, cross_entropy), aes(p, cross_entropy), col = "#D55E00") +
# geom_line(data = data.frame(p, classification_error), aes(p, classification_error), col = "#009E73") +
# geom_text(x = c(0.5, 0.5, 0.5), y = c(0.72, 0.52, 0.3), aes(label = label), data.frame(label = c("Cross Entropy", "Gini Index", "Classification Error"))) +
# ylab("Gini Index, Cross Entropy, Classification Error"))

set.seed(1)
X1 = c(runif(300, -1, 0), runif(200, 0, 1), runif(200, 0, 1), runif(500, -1, 1), runif(1000, 1, 3))
X2 = c(runif(300, -1, 1), runif(200, -1, 0), runif(200, 0, 1), runif(500, 1, 3), runif(1000, -1, 3))
y = c(rep(10, 300), rep(20, 200), rep(30, 200), rep(40, 500), rep(50, 1000))

col = c(rep("#E69F00", 300), rep("#009E73", 200), rep("#0072B2", 200), rep("#D55E00", 500), rep("#CC79A7", 1000))
# p1 = ggplot() + geom_point(data = data.frame(X1, X2), aes(X1, X2), col = col)

library(rpart.plot)
library(rpart)
fit = rpart(y ~ X1 + X2)
# rpart.plot(fit, branch = 1, under = TRUE, faclen = 0, cex = 0.9, main = "Regression Tree")

plot(ggplot() + geom_line(data = data.frame(x = c(-2, -2), y = c(-3, 3)), aes(x, y)) +
geom_line(data = data.frame(x = c(-2, 2), y = c(3, 3)), aes(x, y)) +
geom_line(data = data.frame(x = c(2, 2), y = c(3, -3)), aes(x, y)) +
geom_line(data = data.frame(x = c(2, -2), y = c(-3, -3)), aes(x, y)) +
geom_line(data = data.frame(x = c(-2, 2), y = c(1, 1)), aes(x, y)) +
geom_line(data = data.frame(x = c(-2, 2), y = c(2, 2)), aes(x, y)) +
geom_line(data = data.frame(x = c(0, 0), y = c(2, 1)), aes(x, y)) +
geom_line(data = data.frame(x = c(1, 1), y = c(1, -3)), aes(x, y)) +
geom_text(x = c(0, -1, 1, -0.5, 1.5), y = c(2.5, 1.5, 1.5, -1, -1), aes(label = label), data.frame(label = c("2.49", "-1.06", "0.21", "-1.8", "0.63"))) +
coord_cartesian(xlim = c(-2, 2), ylim = c(-3, 3)) + xlab("X1") + ylab("X2"))
