################################################### #
#' Script de estudo do K-Means
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

## 1.1 Análise número de clusters usando método do cotovelo/elbow method ----
set.seed(6)
wcss <- vector()
for (i in 1:10){
  wcss[i] <- sum(kmeans(X, i)$withinss)
}


## Olhando gráfico para identificar númeor de clusters
plot(1:10, wcss, type = 'b',
     main = paste0('Clusters of clients'),
     xlab = 'Number of clusters', 
     ylab = 'WCSS')


# 2 Modelo ----
## 2.1 K-Means ----
set.seed(29)
cluster_model <- kmeans(X, centers = 5, iter.max = 300, nstart = 10)

## Valores de cluster preditos
y_cluster_model <- cluster_model$cluster

## Novo dataframe com valores de agrupamento
data_cluster <- as.data.frame(X) |> 
  dplyr::mutate(cluster = y_cluster_model)

## 3 Resultados gráficos ----
library(cluster)

## Gráfico de variáveis com agrupamento (mas sem bordas e centro do cluster)
ggplot(data_cluster, aes(x = `Annual.Income..k..`, y = `Spending.Score..1.100.`,
                         color = cluster,  )) +
  geom_point() + 
  scale_colour_gradientn(colours=rainbow(4))

## Gráfico de variáveis feio, mas mais completo (com bordas de cluster)
clusplot(X, cluster_model$cluster, lines = 0,
         shade = F, color = T, labels = 0,
         plotchar = T, span = F,
         main = 'Cluster of clients', x_lab = 'Annual Income', ylab = 'Spending Score')
