######################################################### #
#' Script de estudo do método de aprendizado por reforço
#' Thompson Sampling
#' 
#' 23/06/22
#' Mikael Marin Coletto
##################################################### #

# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura e manipulação da base ----

df <- read.csv("material/Machine Learning A-Z (Codes and Datasets)/Part 06 - Reinforcement Learning/Section 32 - Upper Confidence Bound (UCB)/R/Ads_CTR_Optimisation.csv")

# 2 - Modelo Thompson Sampling ----

## 2.0.1 - Implementando a seleção randômica ----
#### modelo exemplo de "controle"
N = 10000
d = 10
ads_selected = integer(0)
total_reward = 0
# for (n in 1:N){
#   ad = sample(1:10, 1)
#   ads_selected = append(ads_selected, ad)
#   reward = df[n, ad]
#   total_reward = total_reward + reward
# }

## 2.1 - Thompson Sampling ----
N = 10000
d = 10
ads_selected = integer(0)
total_reward = 0
numbers_of_rewards_1 = integer(d) 
numbers_of_rewards_0 = integer(d)
sums_rewards = integer(d)
for (n in 1:N){
  ad = 0
  max_random = 0
  for(i in 1:d){
    random_beta = rbeta(n = 1,
                        shape1 = numbers_of_rewards_1[i] + 1,
                        shape2 = numbers_of_rewards_0[i] + 1)
    if(random_beta > max_random){
      max_random <- random_beta
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  reward = df[n, ad]
  if(reward == 1){
    numbers_of_rewards_1[ad] = numbers_of_rewards_1[ad] + 1
  }else{
    numbers_of_rewards_0[ad] = numbers_of_rewards_0[ad] + 1
  }
  total_reward = total_reward + reward
}

## 3 - Visualização da seleção de ads (Histograma) ----

hist(ads_selected, 
     col = 'blue',
     title = 'Ads selection Histogram (Thompson Sampling)',
     xlabel = 'Ads',
     ylabel = 'Number of selections')
