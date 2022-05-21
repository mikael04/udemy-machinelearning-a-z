# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura e manipulação da base ----

df <- read.csv("R/2 - Regressão/2.6 - Teste de modelos/Data.csv")
df_reg <- df[,2:3]