#################################################### .
## Script de aprendizado do Random Forest Regressor
#################################################### .
# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)
library(randomForest)

# 1. Leitura e manipulação da base ----

df <- read.csv("material/Machine Learning A-Z (Codes and Datasets)/Part 2 - Regression/Section 7 - Support Vector Regression (SVR)/R/Position_Salaries.csv")
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
### neste caso, Random Forest Regressor
set.seed(1234)
typeof(df_aux[1]) ## -> Para o argumento X (variavel independente) precisamos deste formato de lista
typeof(df_aux$Salary) ## -> Para o argumento y (variavel dependente) precisamos deste formato de vetor
regressor <- randomForest(x = df_aux[1], y = df_aux$Salary,
                          ntree = 500)
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
### 4.1 Prevendo resultado com Random Forest Regressor ----
df_predict <- data.frame(Level = 6.5)
y_pred <- predict(regressor, newdata = df_predict)

## 4.2 Criando um df com valor de variavel dependente e independente
predicted <- ncol(df_predict) + 1
df_predict[,predicted] = y_pred
df_predict <- df_predict |> 
  dplyr::rename(Predicted = V2)
