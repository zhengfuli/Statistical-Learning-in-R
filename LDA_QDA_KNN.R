library("ISLR")
# print(summary(Weekly))
# pairs(Weekly)

attach(Weekly)
glm.fit = glm(Direction ~ Lag5, family = binomial())
# print(Today)
print(summary(glm.fit))

glm.probs = predict(glm.fit, type = 'response')

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

confusion_matrix_info(NULL, glm.probs, Direction)

# set.seed(1)
# test_year = sample(1990:2010, size = 2)
# test_year = c(2009, 2010)
# test_data = Weekly[Year == test_year[1] | Year == test_year[2],]
# train_data = Weekly[Year != test_year[1] & Year != test_year[2],]

# glm.fit1 = glm(Direction ~ Lag2, data = train_data, family = binomial)
# glm.probs1 = predict(glm.fit1, test_data, type = 'response')
# confusion_matrix_info(NULL, glm.probs1, test_data$Direction)

# library(MASS)
# lda.fit = lda(Direction ~ Lag2, data = train_data)
# lda.pred = predict(lda.fit, test_data, type = 'response')
# confusion_matrix_info(lda.pred$class, NULL, test_data$Direction)

# qda.fit = qda(Direction ~ Lag2, data = train_data)
# qda.pred = predict(qda.fit, test_data, type = 'response')
# confusion_matrix_info(qda.pred$class, NULL, test_data$Direction)

# library(class)
# set.seed(1)
# knn.pred = knn(as.matrix(train_data$Lag2), as.matrix(test_data$Lag2), train_data$Direction, k = 1)
# confusion_matrix_info(knn.pred, NULL, test_data$Direction)