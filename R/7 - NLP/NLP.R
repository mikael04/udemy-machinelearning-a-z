##################################################################### #
#' Script de estudo do método de processamento de linguagem natural 
#' NLP
#' 
#' 29/06/22
#' Mikael Marin Coletto
##################################################################### #

# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura e manipulação da base ----

df_original <- read.delim("material/Machine Learning A-Z (Codes and Datasets)/Part 07 - Natural Language Processing/Section 36 - Natural Language Processing/Python/Restaurant_Reviews.tsv",
                          quote = '', stringsAsFactors = F)

## 1.1 Pré-processamento ----
### 1.1.1 Limpando texto
library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(df_original$Review))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
## extraindo apenas a raiz/núcleo da palavra
corpus = tm_map(corpus, stemDocument)

corpus = tm_map(corpus, stripWhitespace)

## 1.2 Criando Bag of Words
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)

#### Organizando no formato padrão que uso para os modelos
df = as.data.frame(as.matrix(dtm))
df$Liked = df_original$Liked

# 2 - Modelo Random Forest ----

# 2.0.1 Encoding feature target como factor ----
df$Liked <- factor(df$Liked, levels = c(0,1))

## 2.0.2 Divisão de treino e teste ----
library(caTools)

set.seed(seed = 123)

split = sample.split(df$Liked, SplitRatio = 0.8)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == F)

## Não é preciso aplicar
# ## 1.2 Feature Scaling ----
# training_set[, 1:2] = scale(training_set[,1:2])
# test_set[,1:2] = scale(test_set[,1:2])

# 2. Modelos ----
## 2.1 Random Forest Classifier ----
library(randomForest)

## Não funcionou pra esse modelo com bag of words
# classifier = randomForest(formula = Liked ~ .,
#                           data = training_set,
#                           ntree = 10)
## Outra forma
classifier = randomForest::randomForest(x = training_set[-692],
                                        y = training_set$Liked,
                                        ntree = 10)
summary(classifier)

# 3 - Resultados
## 3.1 Predição no grupo de teste ----
y_pred <- predict(classifier, newdata = test_set[-692], type = 'class')
predicted <- ncol(test_set) + 1
# test_set[,predicted] = y_pred

column_predicted <- paste0("V", predicted)
predicted_column_name <- "Pred_Purchased"
test_set_ren <- test_set
test_set_ren[, predicted] <- y_pred

colnames(test_set_ren)[4] <- predicted_column_name


### 3.2 Fazendo a matrix de confusão
# cm = table(test_set[,3], y_pred)
# cm
## Matrix de confusao e mais algumas medidas avaliando performance do modelo
caret::confusionMatrix(test_set[,692], y_pred)
