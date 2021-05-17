#################################
####### Usando pipes no R #######
#################################

getwd()
setwd('C:/Users/lucas.lima/Documents/Arquivos RStudio/primeiros-passos-r/Aula 6')
getwd()

library(tidyverse)
install.packages('babynames')
library(babynames)

## Exemplo 01

# Sem o pipe '%>%'
diamonds = ggplot2::diamonds
diamonds2 = mutate(diamonds, price_per_carat = price / carat)

# Utilizando o pipe '%>%'
diamonds2 = diamonds %>%
  mutate(price_per_carat = price / carat)

## Exemplo 02
babynames
