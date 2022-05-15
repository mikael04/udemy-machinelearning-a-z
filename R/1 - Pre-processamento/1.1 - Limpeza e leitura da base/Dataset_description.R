## Script para ler e fazer uma descritiva do banco

# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura da base ----

df <- read.csv("data-raw/dataset-description/Data.csv")

## df auxiliar

df_aux <- df

# 2. Manipulando a base ----
## 2.1 Substituindo na pela média da coluna (variáveis numéricas) ----
df_aux$Age = ifelse(is.na(df_aux$Age), 
                    ave(df_aux$Age, FUN = function(x) mean(x, na.rm = T)), df_aux$Age)

df_aux$Salary = ifelse(is.na(df_aux$Salary), 
                       ave(df_aux$Salary, FUN = function(x) mean(x, na.rm = T)), df_aux$Salary)

## 2.2 Encoding de variáveis categóricas ----
## País
unique_countries <- unique(df_aux$Country)
df_aux$Country = factor(df_aux$Country,
                        levels = unique_countries,
                        # levels = c("France", 'Spain', 'Germany'),
                        labels = 1:length(unique_countries))
## Purchased
unique_purch <- unique(df_aux$Purchased)
df_aux$Purchased = factor(df_aux$Purchased,
                          levels = unique_purch,
                          # levels = c("France", 'Spain', 'Germany'),
                          labels = 0:(length(unique_purch)-1))


# 3. Dividindo dataset e treino e teste ----
library(caTools)

set.seed(seed = 123)

split = sample.split(df_aux$Purchased, SplitRatio = 0.8)
training_set <- subset(df_aux, split == TRUE)
test_set <- subset(df_aux, split == F)

## Featuring scale - Adaptando escalas

training_set[,2:3] <- scale(training_set[,2:3])
test_set[,2:3] <- scale(test_set[,2:3])
