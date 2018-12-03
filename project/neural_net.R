library(caret)
library(RSNNS)
set.seed(1)
data(iris)
#将数据顺序打乱
iris = iris[sample(1:nrow(iris),length(1:nrow(iris))),1:ncol(iris)]
#定义网络输入
irisValues= iris[,1:4]
#定义网络输出，并将数据进行格式转换,将类别变量处理成向量形式
irisTargets = decodeClassLabels(iris[,5])
#从中划分出训练样本和检验样本
#splitForTrainingAndTest将输入值和目标值拆分为训练集和测试集。 得到的是列表
#测试集从数据的结尾获取。如果要对数据进行混洗，则应在调用此函数之前完成。
iris = splitForTrainingAndTest(irisValues, irisTargets, ratio = 0)
#数据标准化
iris = normTrainingAndTestSet(iris)

cv_error = rep(0, 10)
for(i in 1:10)
{
    fold = ((i-1)*nrow(iris$inputsTrain)/10+1):(i*nrow(iris$inputsTrain)/10)
    X_valid = iris$inputsTrain[fold,]
    X_train = iris$inputsTrain[-fold,]
    y_valid = iris$targetsTrain[fold,]
    y_train = iris$targetsTrain[-fold,]

    #利用mlp命令执行前馈反向传播神经网络算法
    # model = mlp(X_train, y_train, size = 5, learnFunc = "Quickprop",
    #             learnFuncParams = c(0.1, 2.0, 0.0001, 0.1), maxit = 100, inputsTest = X_valid,
    #             targetsTest = y_valid)

    model = mlp(X_train, y_train, size = 5, learnFunc = "BackpropBatch",
                learnFuncParams = c(10, 0.1), maxit = 100,
                inputsTest = X_valid, targetsTest = y_valid)

    # model = mlp(X_train, y_train, size = 5, learnFunc = "SCG",
    #             learnFuncParams = c(0, 0, 0, 0),  maxit = 30,
    #             inputsTest = X_valid, targetsTest = y_valid)

    # 利用上面建立的模型进行预测
    pred = predict(model, X_valid)

    # 生成混淆矩阵，观察预测精度
    result = confusionMatrix(y_valid, pred)
    # print(result)
    cv_error[i] = sum(diag(result))/sum(result)
}
print(mean(cv_error))
