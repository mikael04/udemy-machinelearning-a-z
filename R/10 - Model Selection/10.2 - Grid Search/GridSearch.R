# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura e manipulação da base ----

df <- read.csv("material/Machine Learning A-Z (Codes and Datasets)/Part 03 - Classification/Section 14 - Logistic Regression/R/Social_Network_Ads.csv")
df = df[, 3:5]

## Encoding a feature como factor
df$Purchased = factor(df$Purchased, levels = c(0, 1))

## 1.1 Divisão de treino e teste ----
library(caTools)

set.seed(seed = 123)

split = sample.split(df$Purchased, SplitRatio = .75)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == F)

## 1.2 Feature Scaling ----
training_set[, 1:2] = scale(training_set[,1:2])
test_set[,1:2] = scale(test_set[,1:2])

# 2. Modelos ----
# 2.1 SVM ----
library(e1071)
# library(kernlab) ## Testando outra lib

classifier_svm = e1071::svm(formula = Purchased ~.,
                            data = training_set,
                            type = 'C-classification', kernel = 'radial')

# classifier_svm = kernlab::ksvm(Purchased ~.,
#                                data = training_set,
#                                type = "C-bsvc",
#                                kernel = 'rbf')
summary(classifier_svm)

# kernlab::fitted(classifier_svm)
## 2.2 Predição no grupo de teste ----
# y_pred <- kernlab::predict(classifier_svm, type = 'response', newdata = test_set[-3])
y_pred <- predict(classifier_svm, newdata = test_set[-3])
predicted <- ncol(test_set) + 1
# test_set[,predicted] = y_pred

column_predicted <- paste0("V", predicted)
predicted_column_name <- "Pred_Purchased"
test_set_ren <- test_set
test_set_ren[, predicted] <- y_pred

colnames(test_set_ren)[4] <- predicted_column_name

## 2.3 Resultados ----

### 2.3.1 Fazendo a matrix de confusão
cm_table <- test_set_ren[, -2:-1]
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
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = kernlab::predict(classifier_svm, type = 'response', newdata = grid_set)
y_grid = prob_set
plot(set[, -3],
     main = 'SVM Kernel/não linear (Training Set)',
     xlab = 'Age',
     ylab = 'Estimated Salary',
     xlim = range(X1),
     ylim = range(X2)
)
contour(X1, X2, matrix(as.numeric(y_grid),length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid==1, 'springgreen3', 'tomato') )
points(set, pch = 21, bg = ifelse(set[, 3]== 1, 'green4', 'red3'))

#### Usando set de teste
# library('Rfast')
set  = test_set
X1 = seq(min(set[, 1]) -1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) -1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier_svm, type = 'response', newdata = grid_set)
y_grid = prob_set
plot(set[, -3],
     main = 'SVM Kernel/não linear (Test Set)',
     xlab = 'Age',
     ylab = 'Estimated Salary',
     xlim = range(X1),
     ylim = range(X2)
)
contour(X1, X2, matrix(as.numeric(y_grid),length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid==1, 'springgreen3', 'tomato') )
points(set, pch = 21, bg = ifelse(set[, 3]== 1, 'green4', 'red3'))


## 2.4 - K-Fold Cross Validation ----
library(caret)
folds = caret::createFolds(training_set$Purchased, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = svm(formula = Purchased ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial')
  y_pred = predict(classifier, newdata = test_fold[-3])
  cM <- caret::confusionMatrix(test_fold[,3], y_pred)
  return (cM$overall['Accuracy'][[1]])
  # cm = table(test_fold[, 3], y_pred)
  # accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  # return(accuracy)
})

accuracy = mean(as.numeric(cv))

## 2.5 - Grid Search ----
library(caret)

classifier_svm <- caret::train(form = Purchased ~ ., data = training_set,
                               method = 'svmRadial')
classifier_svm
