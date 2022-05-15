## Script para ler e fazer uma descritiva do banco

# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura da base ----

df <- read.csv("R/2 - Regressão linear/Salary_Data.csv")

## df auxiliar

df_aux <- df

# 2 Criando modelo ----
## 2.1 Dividindo dataset e treino e teste ----
library(caTools)

set.seed(seed = 123)

split = sample.split(df$Salary, SplitRatio = 2/3)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == F)

## 2.2 Featuring scale - Adaptando escalas ----

# training_set[,2:3] <- scale(training_set[,2:3])
# test_set[,2:3] <- scale(test_set[,2:3])
#
## 2.3 Ajustando regresão linear simples ao grupo de treinamento ----

regressor <- lm(formula = Salary ~ YearsExperience, training_set)

## 2.4 Predição no grupo de teste ----
y_pred <- predict(regressor, newdata = test_set)

test_set[,3] = y_pred

test_set <- test_set |> 
  dplyr::rename(`Salary(y)` = Salary, `Salary(yp)` = V3)

## 2.5 Gráficos dedo modelo ----

plot <- ggplot2::ggplot(data = training_set) +
  geom_point(aes(x = YearsExperience, y = Salary),
             colour = 'red') +
  geom_line(data = training_set, aes(x = YearsExperience,
                                     y = predict(regressor, newdata = training_set)),
            color = 'blue') +
  scale_y_continuous(labels = scales::label_number_si()) +
  xlab('Anos de experiência') +
  ylab('Salário') +
  ggtitle("Salário vs Experiência (Regressão linear para base de treino)") +
  theme_minimal()

plot

plot_test <- ggplot2::ggplot(data = test_set) +
  geom_point(aes(x = YearsExperience, y = `Salary(y)`),
             colour = 'red') +
  geom_line(data = training_set, aes(x = YearsExperience,
                                     y = predict(regressor, newdata = training_set)),
            color = 'blue') + 
  scale_y_continuous(labels = scales::label_number_si()) +
  theme(axis.title.x = 'Anos de experiência', axis.title.y = 'Salário', ) +
  ggtitle("Salário vs Experiência (Regressão linear para base de teste)") +
  theme_minimal()
  

plot_test

