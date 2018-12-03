names(iris)
print(summary(iris))
fix(iris)

library(ggplot2)
library(GGally)

p = ggscatmat(iris, columns = 1:4, color = "Species", alpha = 0.7)
print(p)

attach(iris)
set.seed(1)

library(caTools)
irisValues<- iris[sample(1:nrow(iris),length(1:nrow(iris))),1:ncol(iris)]
colAUC(irisValues[,1:4], irisValues[,5], plotROC=TRUE)