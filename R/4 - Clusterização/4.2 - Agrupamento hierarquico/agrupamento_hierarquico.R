################################################### #
#' Script de estudo do método de agrupamento hierárquico
#' 
#' 14/06/22
#' Mikael Marin Coletto
################################################### #

# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura e manipulação da base ----

df <- read.csv("material/Machine Learning A-Z (Codes and Datasets)/Part 04 - Clustering/Section 24 - K-Means Clustering/Python/Mall_Customers.csv")
## Variáveis usadas para agrupamentos, a princípio usaremos duas
## para facilitar as visualizações e os estudos
X = df[, 4:5]

# 1.0.1 Encoding feature target como factor ----
# df$Purchased <- factor(df$Purchased, levels = c(0,1))

## 1.1 Análise número de clusters ----
dendrogram = hclust(dist(X, method = 'euclidean'), method = 'ward.D')

## Olhando gráfico para identificar númeor de clusters
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers', 
     ylab = 'Euclidean distances')

## Separando grupos e visualizando novo dendograma
plot(dendrogram)
rect.hclust(dendrogram , k = 5, border = 2:6)
abline(h = 3, col = 'red')

# 2 Modelo ----
## 2.1 Agrupamento Hierárquico ----

cluster_model <- hclust(dist(X, method = 'euclidean'), method = 'ward.D')

## Valores de cluster preditos
y_cluster_model <- cutree(cluster_model, 5)

## Novo dataframe com valores de agrupamento
data_cluster <- as.data.frame(X) |> 
  dplyr::mutate(cluster = y_cluster_model)

## 3 Resultados gráficos ----

## Gráfico de variáveis com agrupamento
ggplot(data_cluster, aes(x = `Annual.Income..k..`, y = `Spending.Score..1.100.`,
                         color = cluster,  )) +
  geom_point() + 
  scale_colour_gradientn(colours=rainbow(4))


## Gráfico de variáveis feio, mas mais completo (com bordas de cluster)
clusplot(X, y_cluster_model, lines = 0,
         shade = F, color = T, labels = 0,
         plotchar = T, span = F,
         main = 'Cluster of clients', x_lab = 'Annual Income', ylab = 'Spending Score')