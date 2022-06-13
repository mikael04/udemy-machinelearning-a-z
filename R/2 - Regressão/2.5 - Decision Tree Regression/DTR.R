################################################ .
## Script de aprendizado do SVR
################################################ .
# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)
library(rpart)

# 1. Leitura e manipulação da base ----

df <- read.csv("material/Machine Learning A-Z (Codes and Datasets)/Part 02 - Regression/Section 6 - Polynomial Regression/R/Position_Salaries.csv")
df_aux <- df[,2:3]

## 1.1 Encoding (variavel dependente, X) ----
## se necessárioc
# unique_y <- unique(df_aux$State)
# df_aux$State = factor(df_aux$State,
#                   levels = unique_y,
#                   labels = 1:(length(unique_y)))

# 2 Criando modelo ----

## 2.1 Divisão de treino e teste ----
# library(caTools)
# 
# set.seed(seed = 123)
# 
# split = sample.split(df_aux$Profit, SplitRatio = .8)
# training_set <- subset(df_aux, split == TRUE)
# test_set <- subset(df_aux, split == F)
## 2.1a Neste caso usaremos toda a base para fazer a predição
# training_set <- df_aux

## Neste caso não é necessário (apenas uma variável dependente)
## 2.2 Featuring scale - Adaptando escalas ----
#
# training_set[,2:3] <- scale(training_set[,2:3])
# test_set[,2:3] <- scale(test_set[,2:3])

## 2.3.1 Modelo utilizado ----
### neste caso, Decision Tree Regressor

regressor <- rpart(formula = Salary ~ ., data = df_aux,
                   control = rpart.control(minsplit = 1)) ## Assim colocamos todas as variáveis
summary(regressor)


### 3 Gráficos do modelo linear ----
df_X_grid <- as.data.frame(seq(min(df_aux$Level), max(df_aux$Level), by = 0.01))
colnames(df_X_grid) <- "Level"

predict(regressor, newdata = data.frame(df_X_grid))
plot1 <- ggplot2::ggplot() +
  geom_point(data = df_aux, aes(x = `Level`, y = Salary),
             colour = 'red') +
  geom_line(data = df_X_grid,
            aes(x = Level,
                y = predict(regressor, newdata = df_X_grid)),
            color = 'blue') +
  scale_y_continuous(labels = scales::label_number_si()) +
  xlab('') +
  ylab('Salário') +
  ggtitle("Salário de acordo com cargo (Decision Tree Regression)") +
  theme_minimal()

plot1

## 4 Prevendo resultados ----
### 4.1 Prevendo resultado com regressão linear ----
df_predict <- data.frame(Level = 6.5)
y_pred <- predict(regressor, newdata = df_predict)

## 4.2 Criando um df com valor de variavel dependente e independente
predicted <- ncol(df_predict) + 1
df_predict[,predicted] = y_pred
df_predict <- df_predict |> 
  dplyr::rename(Predicted = V2)

### 4.2 Prevendo resultado com regressão polinomial ----
# x <- 6.5
# df_predict_poli <- data.frame(Level = 6.5, Level2 = 6.5^2, Level3 = 6.5^3, Level4 = 6.5^4)
# y_pred <- predict(regressor_poli, newdata = df_predict_poli)
# predicted <- ncol(df_predict_poli) + 1
# df_predict_poli_p = df_predict_poli
# df_predict_poli_p[,predicted] = y_pred
