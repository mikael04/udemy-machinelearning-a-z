########################################################## #
#' Script de estudo de Redução de dimensionalidade
#' Utilizando o método PCA (Principal Components Analysis)
#' 
#' 21/07/22
#' Mikael Marin Coletto
########################################################## #

# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura e manipulação da base ----

df <- read.csv("material/Machine Learning A-Z (Codes and Datasets)/Part 09 - Dimensionality Reduction/Section 43 - Principal Component Analysis (PCA)/R/Wine.csv")

### 1.0.1 Encoding feature target como factor ----
df$Customer_Segment <- factor(df$Customer_Segment, levels = c(1,2,3))

## 1.1 Divisão de treino e teste ----
library(caTools)

set.seed(seed = 123)

split = sample.split(df$Customer_Segment, SplitRatio = .8)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == F)

## 1.2 Feature Scaling ----
# training_set[-14] = scale(training_set[-14])
training_set[, 1:13] = scale(training_set[,1:13])
test_set[, 1:13] = scale(test_set[, 1:13])

## 1.3 PCA ----
library(e1071)
library(caret)

pca <- caret::preProcess(training_set[-14], method = "pca", pcaComp = 2)
training_set = predict(pca, training_set)
training_set = training_set[c(2, 3, 1)]
pca <- caret::preProcess(test_set[-14], method = "pca", pcaComp = 2)
test_set = predict(pca, test_set)
test_set = test_set[c(2, 3, 1)]

# 2. Modelos ----
## 2.1 SVM ----
library(e1071)

classifier_svm = e1071::svm(formula = Customer_Segment ~.,
                            data = training_set,
                            type = 'C-classification', kernel = 'linear')
summary(classifier_svm)

## 2.2 Predição no grupo de teste ----
y_pred <- predict(classifier_svm, type = 'response', newdata = test_set[-3])
predicted <- ncol(test_set) + 1
# test_set[,predicted] = y_pred

column_predicted <- paste0("V", predicted)
predicted_column_name <- "Pred_Customer_Segment"
test_set_ren <- test_set
test_set_ren[, predicted] <- y_pred

colnames(test_set_ren)[4] <- predicted_column_name

## 2.3 Resultados ----

### 2.3.1 Fazendo a matrix de confusão
cm = table(test_set[,3], y_pred)
cm
## Matrix de confusao e mais algumas medidas avaliando performance do modelo
caret::confusionMatrix(as.factor(test_set[,3]), y_pred)

### 2.3.2 Visualizando resultados
#### Usando set de treino
# library('Rfast')
set  = training_set
X1 = seq(min(set[, 1]) -1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) -1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
prob_set = predict(classifier_svm, newdata = grid_set)
y_grid = prob_set
plot(set[, -3],
     main = 'SVM (Training Set)',
     xlab = 'PCA1',
     ylab = 'PCA2',
     xlim = range(X1),
     ylim = range(X2)
)
contour(X1, X2, matrix(as.numeric(y_grid),length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid==1, 'springgreen3', ifelse(y_grid==2, 'skyblue', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3]== 1, 'green4', ifelse(set[, 3]== 2, 'blue3', 'red3')))

#### Usando set de teste
# library('Rfast')
set  = test_set
X1 = seq(min(set[, 1]) -1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) -1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
prob_set = predict(classifier_svm, type = 'response', newdata = grid_set)
y_grid = prob_set
plot(set[, -3],
     main = 'SVM (Test Set)',
     xlab = 'PCA1',
     ylab = 'PCA2',
     xlim = range(X1),
     ylim = range(X2)
)
contour(X1, X2, matrix(as.numeric(y_grid),length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid==1, 'springgreen3', ifelse(y_grid==2, 'skyblue', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3]== 1, 'green4', ifelse(set[, 3]== 2, 'blue3', 'red3')))


# ## 2.3 Regressão logística ----
# 
# classifier_logReg = glm(formula = Customer_Segment ~., family = binomial,
#                         data = training_set)
# summary(classifier_logReg)
# 
# ### 2.1 Predição no grupo de teste ----
# y_pred_logReg <- round(predict(classifier_logReg, newdata = test_set[-3]))
# predicted <- ncol(test_set) + 1
# # test_set[,predicted] = y_pred_logReg
# 
# column_predicted <- paste0("V", predicted)
# predicted_column_name <- "Pred_Customer_Segment"
# test_set_ren <- test_set
# test_set_ren[, predicted] <- y_pred_logReg
# 
# colnames(test_set_ren)[4] <- predicted_column_name
# 
# ### Fazendo a matrix de confusão
# cm_table <- test_set_ren[, -2:-1]
# cm = table(test_set[,3], y_pred_logReg)
# cm
# 
# ## Matrix de confusao e mais algumas medidas avaliando performance do modelo
# caret::confusionMatrix(test_set_ren[,3], as.factor(y_pred_logReg))
# 
# # Visualizing the training set results
# library('Rfast')
# set  = training_set
# X1 = seq(min(set[, 1]) -1, max(set[, 1]) + 1, by = 0.01)
# X2 = seq(min(set[, 2]) -1, max(set[, 2]) + 1, by = 0.01)
# grid_set = expand.grid(X1, X2)
# colnames(grid_set) = c('Age', 'EstimatedSalary')
# prob_set = predict(classifier_logReg, type = 'response', newdata = grid_set)
# y_grid = ifelse(prob_set > 0.5, 1, 0)
# plot(set[, -3],
#      main = 'Reg Logística (Training Set)',
#      xlab = 'PCA2',
#      ylab = 'PCA2',
#      xlim = range(X1),
#      ylim = range(X2)
# )
# contour(X1, X2, matrix(as.numeric(y_grid),length(X1), length(X2)), add = TRUE)
# points(grid_set, pch = '.', col = ifelse(y_grid==1, 'springgreen3', 'tomato') )
# points(set, pch = 21, bg = ifelse(set[, 3]== 1, 'green4', 'red3'))
# 
# # Visualizing the test set results
# library('Rfast')
# set  = test_set
# X1 = seq(min(set[, 1]) -1, max(set[, 1]) + 1, by = 0.01)
# X2 = seq(min(set[, 2]) -1, max(set[, 2]) + 1, by = 0.01)
# grid_set = expand.grid(X1, X2)
# colnames(grid_set) = c('Age', 'EstimatedSalary')
# prob_set = predict(classifier_logReg, type = 'response', newdata = grid_set)
# y_grid = ifelse(prob_set > 0.5, 1, 0)
# plot(set[, -3],
#      main = 'Reg Logística (Training Set)',
#      xlab = 'PCA2',
#      ylab = 'PCA2',
#      xlim = range(X1),
#      ylim = range(X2)
# )
# contour(X1, X2, matrix(as.numeric(y_grid),length(X1), length(X2)), add = TRUE)
# points(grid_set, pch = '.', col = ifelse(y_grid==1, 'springgreen3', 'tomato') )
# points(set, pch = 21, bg = ifelse(set[, 3]== 1, 'green4', 'red3'))
