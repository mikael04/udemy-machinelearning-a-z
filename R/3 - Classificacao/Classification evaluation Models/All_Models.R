# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura e manipulação da base ----

df <- read.csv("material/Machine Learning A-Z (Codes and Datasets)/Part 03 - Classification/Section 14 - Logistic Regression/R/Social_Network_Ads.csv")
df = df[, 3:5]

# 1.0.1 Encoding feature target como factor ----
df$Purchased <- factor(df$Purchased, levels = c(0,1))

## 1.1 Divisão de treino e teste ----
library(caTools)

set.seed(seed = 123)

split = sample.split(df$Purchased, SplitRatio = .75)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == F)

## 1.2 Feature Scaling ----
training_set[, 1:2] = scale(training_set[,1:2])
test_set[,1:2] = scale(test_set[,1:2])

# 2 Modelos ----
## 2.1 Regressão logística ----
### 2.1.1 Modelos ----

classifier_logReg = glm(formula = Purchased ~., family = binomial,
                        data = training_set)
summary(classifier_logReg)

### 2.1.2 Predição no grupo de teste
y_pred_logReg <- round(predict(classifier_logReg, type = 'response', newdata = test_set[-3]))

# ### 2.1.3 Resultados ----
# 
# ### 2.1.3.1 Fazendo a matrix de confusão
# ## Matrix de confusao e mais algumas medidas avaliando performance do modelo
# caret::confusionMatrix(test_set[,3], y_pred_logReg)

## 2.2. KNN ----
### 2.2.1, 2.2.2 Modelo e predição no grupo de teste ----

y_pred_fac = class::knn(train = training_set[,-3],
                        test = test_set[,-3],
                        cl = training_set[,3],
                        k = 5)
y_pred_knn <- as.character(y_pred_fac)

# ### 2.2.3 Resultados ----
# 
# ### 2.2.3.1 Fazendo a matrix de confusão
# ## Matrix de confusao e mais algumas medidas avaliando performance do modelo
# caret::confusionMatrix(test_set[,3], y_pred_knn)

## 2.3. SVC Linear  ----
### 2.3.1 Modelo ----

classifier_svc_lin = randomForest::randomForest(formula = Purchased ~.,
                                            data = training_set,
                                            ntree = 10)
summary(classifier_svc_lin)

### 2.3.2 Predição no grupo de teste
y_pred_svc_linear <- predict(classifier_svc_lin, newdata = test_set[-3], type = 'class')

# ### 2.3.3 Resultados ----
# 
# ### 2.3.3.1 Fazendo a matrix de confusão
# ## Matrix de confusao e mais algumas medidas avaliando performance do modelo
# caret::confusionMatrix(test_set[,3], y_pred_svc_linear)

## 2.4 SVC não linear ----
### 2.4.1 Modelo ----

classifier_svc_non_lin = randomForest::randomForest(formula = Purchased ~.,
                                            data = training_set,
                                            ntree = 10)
summary(classifier_svc_non_lin)

### 2.4.2 Predição no grupo de teste
y_pred_svc_nao_linear <- predict(classifier_svc_non_lin, newdata = test_set[-3], type = 'class')

# ### 2.4.3 Resultados ----
# 
# ### 2.4.3.1 Fazendo a matrix de confusão
# ## Matrix de confusao e mais algumas medidas avaliando performance do modelo
# caret::confusionMatrix(test_set[,3], y_pred_svc_nao_linear)

## 2.5 Naive Bayes ----
### 2.5.1 Modelo ----

classifier_nb = randomForest::randomForest(formula = Purchased ~.,
                                            data = training_set,
                                            ntree = 10)
summary(classifier_nb)

### 2.5.2 Predição no grupo de teste
y_pred_nb <- predict(classifier_nb, newdata = test_set[-3], type = 'class')

# ### 2.5.3 Resultados ----
# 
# ### 2.5.3.1 Fazendo a matrix de confusão
# ## Matrix de confusao e mais algumas medidas avaliando performance do modelo
# caret::confusionMatrix(test_set[,3], y_pred_nb)

## 2.6 Decision Tree Classifier ----
### 2.6.1 Modelo ----

classifier_dtc = randomForest::randomForest(formula = Purchased ~.,
                                            data = training_set,
                                            ntree = 10)
summary(classifier_dtc)

### 2.6.2 Predição no grupo de teste
y_pred_dtc <- predict(classifier_dtc, newdata = test_set[-3], type = 'class')

# ### 2.6.3 Resultados ----
# 
# ### 2.6.3.1 Fazendo a matrix de confusão
# ## Matrix de confusao e mais algumas medidas avaliando performance do modelo
# caret::confusionMatrix(test_set[,3], y_pred_dtc)

## 2.7. Random Forest Classifier ----
### 2.7.1 Modelo ----

classifier_rfc = randomForest::randomForest(formula = Purchased ~.,
                                        data = training_set,
                                        ntree = 10)
summary(classifier_rfc)

### 2.7.2 Predição no grupo de teste
y_pred_rfc <- predict(classifier_rfc, newdata = test_set[-3], type = 'class')

# 3. Resultados ----

## 3.1. Regressão logística ----
### 3.1.1 Fazendo a matrix de confusão
## Matrix de confusao e mais algumas medidas avaliando performance do modelo
performance_logReg <- caret::confusionMatrix(test_set[,3], as.factor(y_pred_logReg))
performance_logReg[[3]][1]

## 3.2. KNN ----
### 3.2.1 Fazendo a matrix de confusão
## Matrix de confusao e mais algumas medidas avaliando performance do modelo
performance_knn <- caret::confusionMatrix(test_set[,3], as.factor(y_pred_knn))
performance_knn[[3]][1]

## 3.3. SVC Linear ----
### 3.3.1 Fazendo a matrix de confusão
## Matrix de confusao e mais algumas medidas avaliando performance do modelo
performance_svc_lin <- caret::confusionMatrix(test_set[,3], as.factor(y_pred_svc_linear))
performance_svc_lin[[3]][1]

## 3.4. SVC Não linear ----
### 3.4.1 Fazendo a matrix de confusão
## Matrix de confusao e mais algumas medidas avaliando performance do modelo
performance_svc_non_lin <- caret::confusionMatrix(test_set[,3], as.factor(y_pred_svc_nao_linear))
performance_svc_non_lin[[3]][1]

## 3.5. Naive Bayes Classifier ----
### 3.5.1 Fazendo a matrix de confusão
## Matrix de confusao e mais algumas medidas avaliando performance do modelo
performance_nb <- caret::confusionMatrix(test_set[,3], y_pred_nb)
performance_nb[[3]][1]


## 3.6. Decision Tree Classifier ----
### 3.6.1 Fazendo a matrix de confusão
## Matrix de confusao e mais algumas medidas avaliando performance do modelo
performance_dtc <- caret::confusionMatrix(test_set[,3], y_pred_dtc)
performance_dtc[[3]][1]


## 3.7. Random Forest Classifier ----
### 3.7.1 Fazendo a matrix de confusão
performance_rfc <- caret::confusionMatrix(test_set[,3], y_pred_rfc)
performance_rfc[[3]][1]

