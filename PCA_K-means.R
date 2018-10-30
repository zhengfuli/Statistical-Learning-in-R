set.seed(1)
class_1 = matrix(rnorm(20*50, mean = 0, sd = 0.8), ncol = 50)
class_2 = matrix(rnorm(20*50, mean = 0.8, sd = 1), ncol = 50)
class_3 = matrix(rnorm(20*50, mean = 1.5, sd = 1.2), ncol = 50)

x = rbind(scale(class_1), scale(class_2), scale(class_3))
# print(dim(x))
pca = prcomp(x, scale = T)
# print(pca$x[,1:2])

library(ggplot2)
col = c(rep("#009E73", 20), rep("#0072B2", 20), rep("#D55E00", 20))
pca_plot = as.data.frame(pca$x)
p = ggplot() + geom_point(aes(x = pca_plot[,1], y = pca_plot[,2]), col = col, size = 3, shape = 1) + labs(x = "PCA1", y = "PCA2")

x = rbind(scale(class_1), scale(class_2), scale(class_3))
kmeans = kmeans(x, center = 3)
print(kmeans$cluster)

