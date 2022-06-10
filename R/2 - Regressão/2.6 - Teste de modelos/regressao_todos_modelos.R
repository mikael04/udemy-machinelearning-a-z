# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura e manipulação da base ----

df <- read.csv("R/2 - Regressão/2.6 - Teste de modelos/Data.csv")

## 1.1 Divisão de treino e teste ----
library(caTools)

set.seed(seed = 123)

split = sample.split(df$PE, SplitRatio = .8)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == F)
# 2. Modelos ----

## 2.1 Regresão linear múltipla ----
regressor_mult_lin <- lm(formula = PE ~ ., training_set)
summary(regressor_mult_lin)

### 2.1.1 Predição no grupo de teste ----
y_pred_mult_lin <- predict(regressor_mult_lin, newdata = test_set)
predicted <- ncol(test_set) + 1
test_set[,predicted] = y_pred_mult_lin

column_predicted <- paste0("V", predicted)
test_set_full <- test_set |> 
  dplyr::rename(Profit_predicted = !!as.name(column_predicted))

## 2.2 Regresão polinomial ----
df_reg <- df
### Exemplo apenas com uma variável, portanto não consegui replicar o modelo com muitas variáveis
# regressor_poli2 <- lm(formula = Salary ~ polym(Level), df_reg) ## Assim colocamos todas as variáveis
# summary(regressor_poli2)
# df_reg$Level2 <- df_reg$Level^2
# df_reg$Level3 <- df_reg$Level^3
# df_reg$Level4 <- df_reg$Level^4
# regressor_poli <- lm(formula = Salary ~ ., df_reg) ## Assim colocamos todas as variáveis
# summary(regressor_poli)

## 2.3 SVR (support vector regression) ----
library(e1071)

# regressor <- lm(formula = Profit ~ R.D.Spend + Administration + Markting.Spend + State, training_set)
regressor_svm <- svm(formula = PE ~ ., data = training_set,
                 type = 'eps-regression',
                 kernel = 'radial') ## Assim colocamos todas as variáveis
summary(regressor_svm)
y_pred_svr <- predict(regressor_svm, newdata = test_set)
# comparission <- data.frame(y_pred_svr, test_set$PE)
comparission <- data.frame(factor(y_pred_svr), factor(test_set$PE))

u <- union(y_pred_svr, test_set$PE)
t <- table(factor(y_pred_svr, u), factor(test_set$PE, u))

caret::confusionMatrix(t)
# caret::confusionMatrix(table(y_pred_svr, test_set$PE))

caret::MAE(y_pred_svr, test_set$PE)
caret::R2(y_pred_svr, test_set$PE)
caret::RMSE(y_pred_svr, test_set$PE)

# errval <- test_set$PE - y_pred_svr 
# 
# rmse <- function(errval)
# {
#   val = sqrt(mean(errval^2))
#   return(val)
# }  
# rmse(errval)

## 2.4 Decision Tree Regressor ----
library(randomForest)
regressor_dtr <- rpart::rpart(formula = PE ~ ., data = training_set,
                   control = rpart::rpart.control(minsplit = 1)) ## Assim colocamos todas as variáveis
summary(regressor_dtr)

### Prevendo resultado
y_pred_dtr <- predict(regressor_dtr, newdata = test_set)

## 2.5 Random Forest Regressor ----
set.seed(1234)
typeof(training_set$PE) ## -> Para o argumento X (variavel independente) precisamos deste formato de lista
typeof(training_set[,-1]) ## -> Para o argumento y (variavel dependente) precisamos deste formato de vetor
y_train <- training_set$PE
X_train <- training_set[,-ncol(training_set)]
regressor_rfr <- randomForest(x = X_train, y = y_train,
                          ntree = 500)
summary(regressor_rfr)


### Prevendo resultado
y_pred_rfr <- predict(regressor_rfr, newdata = test_set)


## 3. Avaliando modelos ----
## 3.1 Regressão linear ----
caret::R2(y_pred_mult_lin, test_set$PE)
## 3.3 SVR ----
caret::R2(y_pred_svr, test_set$PE)
## 3.4 Decision Tree Regressor ----
caret::R2(y_pred_dtr, test_set$PE)
## 3.5 Random Forest Regressor ----
caret::R2(y_pred_rfr, test_set$PE)
