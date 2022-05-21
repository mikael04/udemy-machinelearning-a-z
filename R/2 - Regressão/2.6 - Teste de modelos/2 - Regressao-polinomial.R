# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura e manipulação da base ----

df <- read.csv("R/2 - Regressão/2.3 - Regressão linear polinomial/Position_Salaries.csv")
df_reg <- df[,2:3]
## 1.1 Encoding (variavel dependente, X) ----
## se necessárioc
# unique_y <- unique(df$State)
# df$State = factor(df$State,
#                   levels = unique_y,
#                   labels = 1:(length(unique_y)))

# 2 Criando modelo ----

## Neste caso usaremos toda a base para fazer a predição
## 2.1 Divisão de treino e teste ----
# library(caTools)
# 
# set.seed(seed = 123)
# 
# split = sample.split(df$Profit, SplitRatio = .8)
# training_set <- subset(df, split == TRUE)
# test_set <- subset(df, split == F)
training_set <- df_reg

## Neste caso não é necessário (apenas uma variável dependente)
## 2.2 Featuring scale - Adaptando escalas ----
#
# training_set[,2:3] <- scale(training_set[,2:3])
# test_set[,2:3] <- scale(test_set[,2:3])

## 2.3.1 Regresão linear ----

# regressor <- lm(formula = Profit ~ R.D.Spend + Administration + Markting.Spend + State, training_set)
regressor_lin <- lm(formula = Salary ~ ., training_set) ## Assim colocamos todas as variáveis
summary(regressor_lin)

## 2.3.2 Regresão polinomial ----
df_reg$Level2 <- df_reg$Level^2
df_reg$Level3 <- df_reg$Level^3
df_reg$Level4 <- df_reg$Level^4
regressor_poli <- lm(formula = Salary ~ ., df_reg) ## Assim colocamos todas as variáveis
summary(regressor_poli)

## Neste caso não testaremos no grupo de teste já que faremos uma predição
## e estamos usando toda a base para criar o modelo
# ## 2.4 Predição no grupo de teste ----
# y_pred <- predict(regressor_1, newdata = test_set)
# predicted <- ncol(test_set) + 1
# test_set[,predicted] = y_pred
# 
# column_predicted <- paste0("V", predicted)
# test_set <- test_set |> 
#   dplyr::rename(Profit_predicted = !!as.name(column_predicted))

### 2.5 Gráficos do modelo linear ----

plot1 <- ggplot2::ggplot(data = df_reg) +
  geom_point(aes(x = `Level`, y = Salary),
             colour = 'red') +
  geom_line(data = df_reg, aes(x = `Level`,
                               y = predict(regressor_lin, newdata = df_reg)),
            color = 'blue') +
  geom_line(data = df_reg, aes(x = `Level`,
                               y = predict(regressor_poli, newdata = df_reg)),
            color = 'green') +
  scale_y_continuous(labels = scales::label_number_si()) +
  xlab('') +
  ylab('Salário') +
  ggtitle("Salário de acordo com cargo") +
  theme_minimal()

plot1

### 2.5 Gráficos do modelo polinomial ----

plot2 <- ggplot2::ggplot(data = df_reg) +
  geom_point(aes(x = `Level`, y = Salary),
             colour = 'red') +
  geom_line(data = df_reg, aes(x = `Level`,
                                     y = predict(regressor_poli, newdata = df_reg)),
            color = 'blue') +
  scale_y_continuous(labels = scales::label_number_si()) +
  xlab('') +
  ylab('Salário') +
  ggtitle("Salário de acordo com cargo") +
  theme_minimal()

plot2

## 3 Prevendo resultados ----
### 3.1 Prevendo resultado com regressão linear ----
df_predict <- data.frame(Level = 6.5)
y_pred <- predict(regressor_lin, newdata = df_predict)
predicted <- ncol(df_predict) + 1
df_predict_lin = df_predict
df_predict_lin[,predicted] = y_pred

### 3.2 Prevendo resultado com regressão polinomial ----
x <- 6.5
df_predict_poli <- data.frame(Level = 6.5, Level2 = 6.5^2, Level3 = 6.5^3, Level4 = 6.5^4)
y_pred <- predict(regressor_poli, newdata = df_predict_poli)
predicted <- ncol(df_predict_poli) + 1
df_predict_poli_p = df_predict_poli
df_predict_poli_p[,predicted] = y_pred
