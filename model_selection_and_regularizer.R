set.seed(1)
X = rnorm(100)
e = rnorm(100)

# Y = 4 - 3 * X + 2 * X^2 - X^3 + e
# Y = 3 + 2 * X -3 * X^2 +0.3* X^3 + e

library(leaps)

# models = regsubsets(y ~ poly(x, 10, raw = T), data = data, nvmax = 10)
# summary = summary(models)

# print(which.min(summary$cp))
# print(which.min(summary$bic))
# print(which.max(summary$adjr2))

# print(coefficients(models, id = 3))
# print(coefficients(models, id = 4))

# Terms = 1:10
# library(ggplot2)
# plot(ggplot() + geom_line(data = data.frame(Terms, summary$cp), aes(Terms, summary$cp), col = "blue"))
# plot(ggplot() + geom_line(data = data.frame(Terms, summary$bic), aes(Terms, summary$bic), col = "green"))
# plot(ggplot() + geom_line(data = data.frame(Terms, summary$adjr2), aes(Terms, summary$adjr2), col = "red"))

# forward = regsubsets(y ~ poly(x, 10, raw = T), data = data, nvmax = 10, method = "forward")
# backward = regsubsets(y ~ poly(x, 10, raw = T), data = data, nvmax = 10, method = "backward")
# forward.summary = summary(forward)
# backward.summary = summary(backward)

# print(which.min(forward.summary$cp))
# print(which.min(forward.summary$bic))
# print(which.max(forward.summary$adjr2))

# print(which.min(backward.summary$cp))
# print(which.min(backward.summary$bic))
# print(which.max(backward.summary$adjr2))

# plot(ggplot() + geom_line(data = data.frame(Terms, forward.summary$cp), aes(Terms, forward.summary$cp), col = "blue"))
# plot(ggplot() + geom_line(data = data.frame(Terms, forward.summary$bic), aes(Terms, forward.summary$bic), col = "green"))
# plot(ggplot() + geom_line(data = data.frame(Terms, forward.summary$adjr2), aes(Terms, forward.summary$adjr2), col = "red"))

# plot(ggplot() + geom_line(data = data.frame(Terms, backward.summary$cp), aes(Terms, backward.summary$cp), col = "blue"))
# plot(ggplot() + geom_line(data = data.frame(Terms, backward.summary$bic), aes(Terms, backward.summary$bic), col = "green"))
# plot(ggplot() + geom_line(data = data.frame(Terms, backward.summary$adjr2), aes(Terms, backward.summary$adjr2), col = "red"))

# print(coefficients(forward, id = 3))
# print(coefficients(forward, id = 4))

# print(coefficients(backward, id = 5))
# print(coefficients(backward, id = 7))

library(glmnet)
# x_matrix = model.matrix(y ~ poly(x, 10, raw = T), data = data)[, -1]
# lasso = cv.glmnet(x_matrix, Y, alpha = 1)
# print(lasso$lambda.min)
# plot(lasso)

# best_model = glmnet(x_matrix, Y, alpha = 1)
# print(predict(best_model, s = lasso$lambda.min, type = "coefficients"))

Y = 4 + 7 * X^7 + e
data = data.frame(y = Y, x = X)

model = regsubsets(y ~ poly(x, 10, raw = T), data = data, nvmax = 10)
summary = summary(model)

print(which.min(summary$cp))
print(which.min(summary$bic))
print(which.max(summary$adjr2))

print(coefficients(model, id = 1))
print(coefficients(model, id = 2))
print(coefficients(model, id = 4))

x_matrix = model.matrix(y ~ poly(x, 10, raw = T), data = data)[, -1]
lasso = cv.glmnet(x_matrix, Y, alpha = 1)
print(lasso$lambda.min)

best_model = glmnet(x_matrix, Y, alpha = 1)
print(predict(best_model, s = lasso$lambda.min, type = "coefficients"))

# plot for lasso geometery illustration in python
# from mpl_toolkits.mplot3d import Axes3D
# import numpy as np
# from matplotlib import pyplot as plt
# from mpl_toolkits.mplot3d.art3d import Poly3DCollection, Line3DCollection
#
# fig = plt.figure()
# ax = Axes3D(fig)
# x = np.arange(0, 4, 0.05)
# y = 4 - x
# X, Y = np.meshgrid(x, y)
# Z = (4-X-Y)**2
#
# x2 = np.arange(-4, 0, 0.05)
# y2 = -4 - x2
# X2, Y2 = np.meshgrid(x2, y2)
# Z2 = (4+X2+Y2)**2
#
# verts = [(0, -2, 0), (-2, 0, 0), (0, 2, 0), (2, 0, 0), (0, -2, 4), (-2, 0, 4), (0, 2, 4), (2, 0, 4)]
# faces = [[0, 1, 2, 3], [4, 5, 6, 7], [0, 1, 5, 4], [1, 2, 6, 5], [2, 3, 7, 6], [0, 3, 7, 4]]
# poly3d = [[verts[vert_id] for vert_id in face] for face in faces]
# x1, y1, z1 = zip(*verts)
# ax.scatter(x1, y1, z1)
# ax.add_collection3d(Poly3DCollection(poly3d, facecolors='w', linewidths=2, alpha=0.1))
# ax.add_collection3d(Line3DCollection(poly3d, colors='k', linewidths=0.8))
#
# ax.plot([-4,4],[0,0],[0,0],label='beta1')
# ax.plot([0,0],[-4,4],[0,0],label='beta2')
# ax.plot([0,0],[0,0],[-2,18],label='RSS')
#
#
# ax.plot_surface(X, Y, Z, rstride=1, cstride=1, cmap='rainbow')
# ax.plot_surface(X2, Y2, Z2, rstride=1, cstride=1, cmap='rainbow')
#
# ax.grid(True)
#
# plt.xlabel('beta1')
# plt.ylabel('beta2')
# ax.set_zlabel('RSS')
#
# plt.xlim((-4, 4))
# plt.ylim((-4, 4))
# plt.show()
