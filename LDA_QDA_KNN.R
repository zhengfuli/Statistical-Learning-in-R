library("ISLR")
# print(summary(Weekly))
# pairs(Weekly)

attach(Weekly)
# print(summary(glm.fit))

confusion_matrix_info = function(pred, probs, ground_truth){
    if(is.null(pred) & !is.null(probs)){
        pred = rep("Up", length(probs))
        pred[probs < 0.5] = "Down"
    }else if(!is.null(pred) & is.null(probs)){
    }else{
        return()
    }

    print(table(pred, ground_truth))

    tp = sum(pred == "Down" & ground_truth == "Down")
    fp = sum(pred == "Down" & ground_truth == "Up")
    tn = sum(pred == "Up" & ground_truth == "Up")
    fn = sum(pred == "Up" & ground_truth == "Down")
    # print(c(tp, fp, tn, fn))

    cm_info = data.frame(values = c(round(1-fp/(fp+tn),2),
                                    round(tp/(tp+fn),2),
                                    round(tp/(tp+fp),2),
                                    round((tp+tn)/(tp+fp+tn+fn),2),
                                    round(2/((tp+fp)/tp+(tp+fn)/tp),2)))
    row.names(cm_info) = c('Specificity', 'Sensitivity/Recall', 'Precision', 'Accuracy', 'F-measure')
    print(cm_info)
}

set.seed(1)
test_year = sample(1990:2010, size = 2)
test_year = c(2009, 2010)
test_data = Weekly[Year == test_year[1] | Year == test_year[2],]
train_data = Weekly[Year != test_year[1] & Year != test_year[2],]

glm.fit1 = glm(Direction ~ Lag1:Lag2 + Lag2, data = train_data, family = binomial)
glm.probs1 = predict(glm.fit1, test_data, type = 'response')
confusion_matrix_info(NULL, glm.probs1, test_data$Direction)

library(MASS)
lda.fit = lda(Direction ~ Lag1:Lag2 + Lag2, data = train_data)
lda.pred = predict(lda.fit, test_data, type = 'response')
confusion_matrix_info(lda.pred$class, NULL, test_data$Direction)

qda.fit = qda(Direction ~ Lag1:Lag2 + Lag2, data = train_data)
qda.pred = predict(qda.fit, test_data, type = 'response')
confusion_matrix_info(qda.pred$class, NULL, test_data$Direction)

library(class)
knn.pred = knn(as.matrix(train_data$Lag2*train_data$Lag1+train_data$Lag2),
               as.matrix(test_data$Lag2*test_data$Lag1+test_data$Lag2),
               train_data$Direction, k = 1)
confusion_matrix_info(knn.pred, NULL, test_data$Direction)

knn.pred1 = knn(as.matrix(train_data$Lag2*train_data$Lag1+train_data$Lag2),
               as.matrix(test_data$Lag2*test_data$Lag1+test_data$Lag2),
               train_data$Direction, k = 10)
confusion_matrix_info(knn.pred1, NULL, test_data$Direction)

knn.pred2 = knn(as.matrix(train_data$Lag2*train_data$Lag1+train_data$Lag2),
               as.matrix(test_data$Lag2*test_data$Lag1+test_data$Lag2),
               train_data$Direction, k = 100)
confusion_matrix_info(knn.pred2, NULL, test_data$Direction)

library(pROC)

pred = rep(0, length(glm.probs1))
pred[glm.probs1 < 0.5] = 1
ground_truth = rep(0, length(test_data$Direction))
ground_truth[test_data$Direction == "Down"] = 1

roc1 = roc(ground_truth, pred)

pred1 = rep(0, length(lda.pred$class))
pred1[lda.pred$class == "Down"] = 1
roc2 = roc(ground_truth, pred1)

pred2 = rep(0, length(qda.pred$class))
pred2[qda.pred$class == "Down"] = 1
roc3 = roc(ground_truth, pred2)

pred3 = rep(0, length(knn.pred))
pred3[knn.pred == "Down"] = 1
roc4 = roc(ground_truth, pred3)

pred4 = rep(0, length(knn.pred1))
pred4[knn.pred1 == "Down"] = 1
roc5 = roc(ground_truth, pred4)

pred5 = rep(0, length(knn.pred2))
pred5[knn.pred2 == "Down"] = 1
roc6 = roc(ground_truth, pred5)

roc_curves = ggroc(list(Logistic = roc1, LDA = roc2, QDA = roc3, KNN_1 = roc4, KNN_10 = roc5, KNN_100 = roc6))
plot(roc_curves, print.auc = TRUE, auc.polygon = TRUE, legacy.axes = TRUE,
     max.auc.polygon = TRUE, print.thres = TRUE)