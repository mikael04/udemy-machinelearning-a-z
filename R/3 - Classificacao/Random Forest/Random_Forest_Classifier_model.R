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

# 2. Modelos ----
# 2.1 Random Forest Classifier ----

classifier = randomForest::randomForest(formula = Purchased ~.,
                                        data = training_set,
                                        ntree = 10)
summary(classifier)

## 2.2 Predição no grupo de teste ----
y_pred <- predict(classifier, newdata = test_set[-3], type = 'class')
predicted <- ncol(test_set) + 1
# test_set[,predicted] = y_pred

column_predicted <- paste0("V", predicted)
predicted_column_name <- "Pred_Purchased"
test_set_ren <- test_set
test_set_ren[, predicted] <- y_pred

colnames(test_set_ren)[4] <- predicted_column_name

## 2.3 Resultados ----

### 2.3.1 Fazendo a matrix de confusão
# cm = table(test_set[,3], y_pred)
# cm
## Matrix de confusao e mais algumas medidas avaliando performance do modelo
caret::confusionMatrix(test_set[,3], y_pred)

### 2.3.2 Visualizando resultados
#### Usando set de treino
# library('Rfast')
set  = training_set
X1 = seq(min(set[, 1]) -1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) -1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'Random Forest Classifier (Training Set)',
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
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'Random Forest Classifier  (Test Set)',
     xlab = 'Age',
     ylab = 'Estimated Salary',
     xlim = range(X1),
     ylim = range(X2)
)
contour(X1, X2, matrix(as.numeric(y_grid),length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid==1, 'springgreen3', 'tomato') )
points(set, pch = 21, bg = ifelse(set[, 3]== 1, 'green4', 'red3'))

## 3.0 Plotando a Random Forest ----
df$Purchased <- factor(df$Purchased, levels = c(0,1))

split = sample.split(df$Purchased, SplitRatio = .75)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == F)

classifier = rpart::rpart(formula = Purchased ~.,
                          data = training_set)
summary(classifier)

plot(classifier)
text(classifier)