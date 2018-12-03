library(e1071)
names(iris)

set.seed(1)
iris$Species = factor(iris$Species, labels = c(1,2,3))
attach(iris)

tuning_linear = tune(svm, Species ~ ., data = iris, kernel = "linear", ranges = list(cost = c(1e-3, 1e-2, 1e-1, 5e-1, 1, 5, 10, 100)))
print(summary(tuning_linear))

tuning_radial = tune(svm, Species ~ ., data = iris, kernel = "radial", ranges = list(cost = c(1e-3, 1e-2, 1e-1, 5e-1, 1, 5, 10, 100), gamma = c(1e-3, 1e-2, 1e-1, 5e-1, 1, 5, 10, 100)))
print(summary(tuning_radial))

tuning_poly = tune(svm, Species ~ ., data = iris, kernel = "polynomial", ranges = list(cost = c(1e-3, 1e-2, 1e-1, 5e-1, 1, 5, 10, 100), degree = c(2, 3, 4)))
print(summary(tuning_poly))