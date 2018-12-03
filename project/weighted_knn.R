names(iris)
# print(summary(iris))
# fix(iris)

library(ggplot2)
library(GGally)

# Plot the data set
# p = ggscatmat(iris, columns = 1:4, color = "Species", alpha = 0.7)
# print(p)

attach(iris)
set.seed(1)

# 10-fold cross-validation knn and weighted knn
library(kknn)
library(class)

knn_misclassification_rate = rep(0, 12)
wknn_misclassification_rate = rep(0, 12)

for(k in 1:12)
{
    # 10-fold cross-validation
    samples = sample(1:nrow(iris))
    knn_cv_error = rep(0, 10)
    wknn_cv_error = rep(0, 10)

    for(i in 1:10)
    {
        fold = ((i-1)*length(samples)/10+1):(i*length(samples)/10)
        valid = samples[fold]
        train = samples[-fold]

        knn = knn(iris[train, -5], iris[valid, -5], iris[train, 5], k = k)
        knn_cv_error[i] = mean(knn != iris[valid,]$Species)

        wknn = kknn(Species ~ ., k = k, iris[train,], iris[valid,], kernel = "optimal")
        wknn_cv_error[i] = mean(wknn$fitted.values != iris[valid,]$Species)
    }

    knn_misclassification_rate[k] = mean(knn_cv_error)
    wknn_misclassification_rate[k] = mean(wknn_cv_error)
}
# print(knn_misclassification_rate)
train = sample(1:nrow(iris))
knn = knn(iris[train, -5], iris[train, -5], iris[train, 5], k = 9)
#
pcol = as.character(as.numeric(iris[train,]$Species))
print(pairs(iris[train,][1:4], pch = pcol, col = c("green3", "red")[(iris[train,]$Species != knn)+1]))
#
# p = ggplot() + geom_line(data = data.frame(k = 1:12, misclassification_rate = knn_misclassification_rate), aes(k, misclassification_rate), col = "#D55E00") +
#     geom_line(data = data.frame(k = 1:12, misclassification_rate = wknn_misclassification_rate), aes(k, misclassification_rate), col = "#0072B2") +
#     scale_x_continuous(breaks = seq(1, 12, 1)) + geom_text(x = c(4, 7), y = c(0.065, 0.04), aes(label = label), data.frame(label = c("Weighted KNN", "KNN")))
#
# plot(p)