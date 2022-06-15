##################################################### #
#' Script de estudo do método de recomendação Eclat
#' 
#' 15/06/22
#' Mikael Marin Coletto
##################################################### #

# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura e manipulação da base ----

# df <- read.csv("R/5 - Recomendação/Market_Basket_Optimisation.csv", header = F)
transactions <- arules::read.transactions("R/5 - Recomendação/Market_Basket_Optimisation.csv", 
                                          sep = ",", rm.duplicates = T)

## 1.1 Visualizando informações iniciais sobre as transações ----
arules::summary(transactions)
arules::itemFrequencyPlot(transactions, topN = 10)

# 2 - Modelo apriori ----

library(arules)

#### Vamos definir o suporte como, 3 compras por dia em 7 dias na semana, dividido
#### pelo número de transações (7501) - 3*7/7500
support <- round(4*7/7500, 3)

## 2.1 Treinando o modelo ----
rules <- arules::eclat(transactions, parameter = list(supp = support, minlen = 2))

# 3 - Resultados ----
## 3.1 Visualizando resultados ----
arules::inspect(arules::sort(rules, by = 'support')[1:10])
