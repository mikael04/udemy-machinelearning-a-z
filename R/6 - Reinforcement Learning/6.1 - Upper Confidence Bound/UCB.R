######################################################### #
#' Script de estudo do método de aprendizado por reforço
#' Upper Confidence Bound
#' 
#' 23/06/22
#' Mikael Marin Coletto
##################################################### #

# 0. Bibliotecas e scripts base ----
library(dplyr)
require(ggplot2)

# 1. Leitura e manipulação da base ----

df <- read.csv("material/Machine Learning A-Z (Codes and Datasets)/Part 06 - Reinforcement Learning/Section 32 - Upper Confidence Bound (UCB)/R/Ads_CTR_Optimisation.csv")

# 2 - Modelo UCB ----

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

## 2.1 - UCB ----
N = 10000
d = 10
ads_selected = integer(0)
total_reward = 0
numbers_of_selection = integer(d)
sums_rewards = integer(d)
for (n in 1:N){
  ad = 0
  max_upper_bound = 0
  for(i in 1:d){
    if(numbers_of_selection[i] > 0){
      average_reward = sums_rewards[i] / numbers_of_selection[i]
      delta_i = sqrt(3/2 * log(n)/numbers_of_selection[i])
      upper_bound =  average_reward + delta_i
    } else{
      upper_bound = 1e400
    }
    if(upper_bound > max_upper_bound){
      max_upper_bound <- upper_bound
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  numbers_of_selection[ad] = numbers_of_selection[ad] + 1
  reward = df[n, ad]
  sums_rewards[ad] = sums_rewards[ad] + reward
  total_reward = total_reward + reward
}

## 3 - Visualização da seleção de ads (Histograma) ----

hist(ads_selected, 
     col = 'blue',
     title = 'Ads selection Histogram (UCB)',
     xlabel = 'Ads',
     ylabel = 'Number of selections')
