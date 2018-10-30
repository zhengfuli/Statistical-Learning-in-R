d = as.dist(matrix(c(0, 0.3, 0.7, 0.4,
                     0.3, 0, 0.8, 0.5,
                     0.5, 0.4, 0.0, 0.45,
                     0.8, 0.7, 0.45, 0.0), nrow=4))
# plot(hclust(d, method="complete"), label=c(2, 1, 4, 3))
# plot(hclust(d, method="single"))

library(ggplot2)
data = data.frame(X1 = c(1,1,0,5,6,4), X2 = c(4,3,4,1,2,0))
p = ggplot(data = data, aes(X1, X2)) + geom_point(size = 3)
# plot(p)

set.seed(1)
rand_clus = sample(2, 6, replace = T)

cluster_1 = data.frame(X1 = data$X1[which(rand_clus == 1)], X2 = data$X2[which(rand_clus == 1)])
cluster_2 = data.frame(X1 = data$X1[which(rand_clus == 2)], X2 = data$X2[which(rand_clus == 2)])
centroid_1 = data.frame(X1 = mean(cluster_1$X1), X2 = mean(cluster_1$X2))
centroid_2 = data.frame(X1 = mean(cluster_2$X1), X2 = mean(cluster_2$X2))

clustering = function(centroid_1, centroid_2)
{
    labels = rep(0, length(data$X1))
    for(i in 1:length(data$X1))
    {
        obs = data.frame(X1 = data$X1[i], X2 = data$X2[i])
        labels[i] = ifelse(sqrt((obs$X1 - centroid_1$X1)^2 + (obs$X2 - centroid_1$X2)^2) > sqrt((obs$X1 - centroid_2$X1)^2 + (obs$X2 - centroid_2$X2)^2), 2, 1)
    }
    return(labels)
}

labels = rand_clus
while(!all(labels == clustering(centroid_1, centroid_2)))
{   labels = clustering(centroid_1, centroid_2)
    cluster_1 = data.frame(X1 = data$X1[which(labels == 1)], X2 = data$X2[which(labels == 1)])
    cluster_2 = data.frame(X1 = data$X1[which(labels == 2)], X2 = data$X2[which(labels == 2)])
    centroid_1 = data.frame(X1 = mean(cluster_1$X1), X2 = mean(cluster_1$X2))
    centroid_2 = data.frame(X1 = mean(cluster_2$X1), X2 = mean(cluster_2$X2))
}

col = c("#FF8247", "#6495ED")[labels]
p = ggplot(data = data, aes(X1, X2)) + geom_point(size = 3, col = col)

p = p + geom_point(data = rbind(centroid_1, centroid_2), aes(X1, X2), col = c("#CD2626", "#000080"), pch = 23, size = 6)
print(rbind(centroid_1,centroid_2))

for(i in 1:length(cluster_1$X1))
{
    p = p + geom_line(data = rbind(centroid_1, data.frame(X1 = cluster_1$X1[i], X2 = cluster_1$X2[i])), linetype = "dashed", col = "#FF8247")
}

for(i in 1:length(cluster_2$X1))
{
    p = p + geom_line(data = rbind(centroid_2, data.frame(X1 = cluster_2$X1[i], X2 = cluster_2$X2[i])), linetype = "dashed", col = "#6495ED")
}
# plot(p)

set.seed(1)
control = matrix(rnorm(1000 * 50), nrow = 1000)
treatment = matrix(rnorm(1000 * 50), nrow = 1000)
X = cbind(control, treatment)
X = X[,sample(100, 100, replace = F)]

error_A = matrix(rnorm(1000 * 50, mean = -1), nrow = 1000)
error_B = matrix(rnorm(1000 * 50, mean = 1), nrow = 1000)
X = X + cbind(error_A, error_B)

X = rbind(X, c(rep(-10, 50), rep(10, 50)))

X[1,] = seq(-27.5, 27.5-0.55, 0.55)
pr = prcomp(scale(X))
print(summary(pr)$importance[,1])