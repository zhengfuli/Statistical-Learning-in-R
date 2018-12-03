import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
from sklearn import datasets
from sklearn.decomposition import PCA
from sklearn import model_selection
from sklearn import svm
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis

iris = datasets.load_iris()

X = iris.data
y = iris.target
target_names = iris.target_names

pca = PCA(n_components=2)
X_r = pca.fit(X).transform(X)

lda = LinearDiscriminantAnalysis(n_components=2)
X_r2 = lda.fit(X, y).transform(X)

# SVM and cv
X_train, X_valid, y_train, y_valid = model_selection.train_test_split(X_r, y, test_size = 0.1, random_state = 0)
clf = svm.SVC(C = 0.1, kernel = 'linear', decision_function_shape = 'ovr')
clf.fit(X_train, y_train.ravel())
print(clf.score(X_valid, y_valid))

# PCA
clf.fit(X_r, y.ravel())
x1_min, x1_max = X_r[:, 0].min(), X_r[:, 0].max()
x2_min, x2_max = X_r[:, 1].min(), X_r[:, 1].max()
x1, x2 = np.mgrid[x1_min:x1_max:200j, x2_min:x2_max:200j]
grid_test = np.stack((x1.flat, x2.flat), axis = 1)
grid_hat = clf.predict(grid_test)
grid_hat = grid_hat.reshape(x1.shape)

cm_light = mpl.colors.ListedColormap(['#A0FFA0', '#FFA0A0', '#A0A0FF'])
cm_dark = mpl.colors.ListedColormap(['g', 'r', 'b'])
plt.figure()
plt.pcolormesh(x1, x2, grid_hat, cmap=cm_light)
plt.scatter(X_r[:, 0], X_r[:, 1], c = y, edgecolors = 'k', s = 50, cmap = cm_dark)
plt.scatter(X_r[:, 0], X_r[:, 1], s = 120, facecolors = 'none', zorder = 10)
# plt.xlabel(u'花萼长度', fontsize = 13)
# plt.ylabel(u'花萼宽度', fontsize = 13)
plt.xlim(x1_min, x1_max)
plt.ylim(x2_min, x2_max)
plt.title(u'Linear SVM (cost = 0.1) on Iris Reduced by PCA', fontsize = 15)

X_train, X_valid, y_train, y_valid = model_selection.train_test_split(X_r2, y, test_size = 0.1, random_state = 0)
clf.fit(X_train, y_train.ravel())
print(clf.score(X_valid, y_valid))
# LDA
clf.fit(X_r2, y.ravel())
x1_min, x1_max = X_r2[:, 0].min(), X_r2[:, 0].max()
x2_min, x2_max = X_r2[:, 1].min(), X_r2[:, 1].max()
x1, x2 = np.mgrid[x1_min:x1_max:200j, x2_min:x2_max:200j]
grid_test = np.stack((x1.flat, x2.flat), axis = 1)
grid_hat = clf.predict(grid_test)
grid_hat = grid_hat.reshape(x1.shape)

cm_light = mpl.colors.ListedColormap(['#A0FFA0', '#FFA0A0', '#A0A0FF'])
cm_dark = mpl.colors.ListedColormap(['g', 'r', 'b'])
plt.figure()
plt.pcolormesh(x1, x2, grid_hat, cmap = cm_light)
plt.scatter(X_r2[:, 0], X_r2[:, 1], c = y, edgecolors = 'k', s = 50, cmap = cm_dark)
plt.scatter(X_r2[:, 0], X_r2[:, 1], s = 120, facecolors = 'none', zorder = 10)
# plt.xlabel(u'花萼长度', fontsize = 13)
# plt.ylabel(u'花萼宽度', fontsize = 13)
plt.xlim(x1_min, x1_max)
plt.ylim(x2_min, x2_max)
plt.title(u'Linear SVM (cost = 0.1) on Iris Reduced by LDA', fontsize = 15)
# plt.grid()
plt.show()