################################################### #
#' Script de estudo de Redes Neurais Artificiais
#' 
#' 01/07/22
#' Mikael Marin Coletto
################################################### #

# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura e manipulação da base ----

df <- read.csv("material/Machine Learning A-Z (Codes and Datasets)/Part 08 - Deep Learning/Section 39 - Artificial Neural Networks (ANN)/Python/Churn_Modelling.csv")
df = df[, 4:14]

# 1.0.1 Encoding variáveis como factor ----
df$Gender <- as.numeric(factor(df$Gender,
                               levels = unique(df$Gender),
                               labels = 1:length(unique(df$Gender))))
df$Geography <- as.numeric(factor(df$Geography,
                                  levels = unique(df$Geography),
                                  labels = 1:length(unique(df$Geography))))
unique(df$Geography)



## 1.1 Divisão de treino e teste ----
library(caTools)

set.seed(seed = 123)

split = sample.split(df$Exited, SplitRatio = .8)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == F)

## 1.2 Feature Scaling ---- 
training_set[, -11] = scale(training_set[,-11])
test_set[,-11] = scale(test_set[,-11])

# 2. Criando o modelo ANN ----
library(h2o)
h2o::h2o.init(nthreads = -1)
classifier = h2o.deeplearning(y = 'Exited', training_frame = as.h2o(training_set),
                              activation = 'Rectifier',
                              hidden = c(6, 6),
                              epochs = 100,
                              train_samples_per_iteration = -2
                              )

## 2.1 Predição no grupo de teste ----
prob_pred <- h2o.predict(classifier, newdata = as.h2o(test_set[-11]))
y_pred <- as.vector(prob_pred > 0.5)
predicted <- ncol(test_set) + 1
# test_set[,predicted] = y_pred

column_predicted <- paste0("V", predicted)
predicted_column_name <- "Pred_Purchased"
test_set_ren <- test_set
test_set_ren[, predicted] <- y_pred

colnames(test_set_ren)[4] <- predicted_column_name

## 2.3 Resultados ----

### 2.3.1 Fazendo a matrix de confusão
# cm = table(test_set[,11], y_pred)
# cm
## Matrix de confusao e mais algumas medidas avaliando performance do modelo
caret::confusionMatrix(as.factor(test_set[,11]), as.factor(y_pred))

h2o.shutdown()
