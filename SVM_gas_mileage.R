library(ISLR)
med = median(Auto$mpg)
new_mpg = ifelse(Auto$mpg > med, 1, 0)
Auto$new_mpg = as.factor(new_mpg)
library(e1071)
set.seed(1)
attach(Auto)
na.omit(Auto)
# fix(Auto)
# tuning = tune(svm, new_mpg ~ ., data = Auto, kernel = "linear", ranges = list(cost = c(.1, .2, .4, .6, .7,.71,.72,.73,.74, .75)))
# print(summary(tuning))

# tuning_radial = tune(svm, new_mpg ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(100), gamma = c(0.01)))
# print(summary(tuning_radial))

# tuning_polynomial = tune(svm, new_mpg ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(122, 123, 123.5, 124, 124.5, 125, 126), degree = c(2, 3, 4)))
# print(summary(tuning_polynomial))

# svm.linear = svm(new_mpg ~ mpg + acceleration, data = Auto, kernel = "linear", cost = 0.72)

# svm.polynomial = svm(new_mpg ~ ., data = Auto, kernel = "polynomial", cost = 100, degree = 2)
# plot(svm.linear, Auto, mpg ~ acceleration, svSymbol="#")
# plotpairs = function(fit) {
#     for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))])
#     {
#         plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
#     }
# }
#
# plotpairs(svm.linear)

svm_plot = function(X1, X2, kernel, cost, gamma, degree)
{
    fit = svm(new_mpg ~ ., data = Auto, kernel = kernel, cost = cost)

    grid_range = apply(Auto[c(X1, X2)], 2, range)
    grid_X1 = seq(from = grid_range[1, 1] - 0.25, to = grid_range[2, 1] + 0.25, length = 75)
    grid_X2 = seq(from = grid_range[1, 2] - 0.5, to = grid_range[2, 2] + 0.5, length = 75)

    grid = expand.grid(acceleration = grid_X1, weight = grid_X2)
    grid$class = predict(fit, grid)
    # decision_values = predict(fit, grid, decision.values = TRUE)
    # grid$z = as.vector(attributes(decision_values)$decision)
    #
    # plot(grid[c(X1, X2)], col = ifelse(grid$class == 1, '#0571B070', '#CA002070'), pch='20', cex=.2)
    # points(Auto[c(X1, X2)], col = ifelse(new_mpg == 1, '#0571B070', '#CA002070'))
    # contour(grid_X1, grid_X2, matrix(grid$z, length(grid_X1), length(grid_X2)), level = 0, lwd = 1.5, drawlabels = FALSE, add = TRUE)
    # mtext(paste('\nKernel:', kernel, '        Cost:', cost, '        Degree:', degree), line = -3, outer = TRUE)
}

svm_plot("acceleration", "weight", "linear", cost = 0.72)