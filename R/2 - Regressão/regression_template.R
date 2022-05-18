# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura e manipulação da base ----

# df <- read.csv("R/2 - Regressão/2.3 - Regressão linear polinomial/Position_Salaries.csv")
# df_aux <- df[,2:3]

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
### neste caso, regressão polinomial

# # regressor <- lm(formula = Profit ~ R.D.Spend + Administration + Markting.Spend + State, training_set)
# regressor_lin <- lm(formula = Salary ~ ., training_set) ## Assim colocamos todas as variáveis
# summary(regressor_lin)
# 
# ## 2.3.2 Regresão polinomial ----
# df_aux$Level2 <- df_aux$Level^2
# df_aux$Level3 <- df_aux$Level^3
# df_aux$Level4 <- df_aux$Level^4
# regressor_poli <- lm(formula = Salary ~ ., df_aux) ## Assim colocamos todas as variáveis
# summary(regressor_poli)


### 3 Gráficos do modelo linear ----
plot1 <- ggplot2::ggplot() +
  geom_point(data = df_aux, aes(x = `Level`, y = Salary),
             colour = 'red') +
  geom_line(data = df_aux,
            aes(x = `Level`,
                y = predict(regressor_lin, newdata = df_aux)),
            color = 'blue') +
  scale_y_continuous(labels = scales::label_number_si()) +
  xlab('') +
  ylab('Salário') +
  ggtitle("Salário de acordo com cargo") +
  theme_minimal()

plot1
### 3 Gráficos do modelo linear + modelo polinomial ----
# x_grid_l1 <- seq(min(df_aux$Level), max(df_aux$Level), 0.1)
plot1 <- ggplot2::ggplot() +
  geom_point(data = df_aux,
             aes(x = `Level`, y = Salary),
             colour = 'red') +
  geom_line(data = df_aux,
            aes(x = `Level`,
                y = predict(regressor_lin, newdata = df_aux)),
            color = 'blue') +
  # geom_line(data = df_aux,
  #           aes(x = `x_grid`,
  #               y = predict(regressor_poli, newdata = data.frame(Level = x_grid))),
  #           color = 'green') +
  geom_line(data = df_aux, aes(x = `Level`,
                               y = predict(regressor_poli, newdata = df_aux)),
            color = 'green') +
  scale_y_continuous(labels = scales::label_number_si()) +
  xlab('') +
  ylab('Salário') +
  ggtitle("Salário de acordo com cargo") +
  theme_minimal()

plot1

## 4 Prevendo resultados ----
### 4.1 Prevendo resultado com regressão linear ----
# df_predict <- data.frame(Level = 6.5)
# y_pred <- predict(regressor_lin, newdata = df_predict)
# predicted <- ncol(df_predict) + 1
# df_predict_lin = df_predict
# df_predict_lin[,predicted] = y_pred

### 4.2 Prevendo resultado com regressão polinomial ----
# x <- 6.5
# df_predict_poli <- data.frame(Level = 6.5, Level2 = 6.5^2, Level3 = 6.5^3, Level4 = 6.5^4)
# y_pred <- predict(regressor_poli, newdata = df_predict_poli)
# predicted <- ncol(df_predict_poli) + 1
# df_predict_poli_p = df_predict_poli
# df_predict_poli_p[,predicted] = y_pred
