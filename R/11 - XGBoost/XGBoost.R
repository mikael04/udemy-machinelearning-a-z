########################################################## #
#' Script de estudo do modelo XGBoost
#' 
#' 27/07/22
#' Mikael Marin Coletto
########################################################## #

# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura e manipulação da base ----

df <- read.csv("material/Machine Learning A-Z (Codes and Datasets)/Part 08 - Deep Learning/Section 39 - Artificial Neural Networks (ANN)/Python/Churn_Modelling.csv")
df = df[, 4:14]

# 1.0.1 Encoding variáveis como factor ----
df$Gender <- as.numeric(factor(df$Gender,
                               levels = unique(df$Gender),
                               labels = 1:length(unique(df$Gender))))
df$Geography <- as.numeric(factor(df$Geography,
                                  levels = unique(df$Geography),
                                  labels = 1:length(unique(df$Geography))))


# df$Exited <- factor(df$Exited, levels = c('0', '1'))


## 1.1 Divisão de treino e teste ----
library(caTools)

set.seed(seed = 123)

split = sample.split(df$Exited, SplitRatio = .8)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == F)

# 2. Criando o modelo com XGBoost ----
library(xgboost)

classifier_xgb <- xgboost::xgboost(data = as.matrix(training_set[-11]),
                                   label = training_set$Exited,
                                   nrounds = 10)
## 2.1 Resultados ----

### Vetor de predição
y_pred = predict(classifier_xgb, newdata = as.matrix(test_set[-11]))
y_pred <- base::ifelse(y_pred >= 0.5, 1, 0)
y_pred <- as.factor(y_pred)
### 2.1.2 Fazendo a matrix de confusão
# cm = table(test_set[,11], y_pred)
# cm
## Matrix de confusao e mais algumas medidas avaliando performance do modelo
caret::confusionMatrix(as.factor(test_set[,11]), y_pred)


## 2.2 - K-Fold Cross Validation com XGBoost ----
library(caret)
folds = caret::createFolds(training_set$Exited, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier_xgb <- xgboost::xgboost(data = as.matrix(training_fold[-11]),
                                 label = training_fold[,11],
                                 nrounds = 10)
  ### Vetor de predição
  y_pred = predict(classifier_xgb, newdata = as.matrix(test_fold[-11]))
  y_pred <- base::ifelse(y_pred >= 0.5, 1, 0)
  y_pred <- as.factor(y_pred)
  cM <- caret::confusionMatrix(as.factor(test_fold[,11]), y_pred)
  return (cM$overall['Accuracy'][[1]])
  # cm = table(test_fold[, 3], y_pred)
  # accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  # return(accuracy)
})

accuracy = mean(as.numeric(cv))
accuracy
