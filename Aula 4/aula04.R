######################################################
####### Tratamento de dados com o pacote dplys #######
######################################################

getwd()
setwd('C:/Users/lucas.lima/Documents/Arquivos RStudio/Aula 4')
getwd()

############################ EXERCICIO 01 ############################

## Pacotes utilizados
install.packages(tidyverse)
library(tidyverse)
install.packages(sidrar)
library(sidrar)
install.packages(tstools)
library(tstools)
install.packages(zoo)
library(zoo)
install.packages(scales)
library(scales)
install.packages(readxl)
library(readxl)


## Coleta dos dados
pib = get_sidra(api = '/t/1620/n1/all/v/all/p/all/c11255/90707/d/v583%202') %>%
  mutate(date = as.yearqtr(`Trimestre (Código)`, format = '%Y%q')) %>%
  rename(pib = Valor) %>%
  mutate(var_interanual = (pib/lag(pib,4)-1)*100) %>%
  # mutate(var_anual = acum_i(pib,4)) %>%
  select(date, pib, var_interanual) %>%
  as_tibble()
class(pib)
View(pib)
colnames(pib)
pib

pib_sa = get_sidra(api = '/t/1621/n1/all/v/all/p/all/c11255/90707/d/v584%202') %>%
  mutate(date = as.yearqtr(`Trimestre (Código)`, format = '%Y%q')) %>%
  rename(pib_sa = Valor) %>%
  mutate(var_marginal = (pib_sa/lag(pib_sa,1)-1)*100) %>%
  select(date, pib_sa, var_marginal) %>%
  as_tibble()
class(pib_sa)
View(pib_sa)
colnames(pib_sa)
pib_sa

data = inner_join(pib, pib_sa, by ='date') %>%
  drop_na()
data

## Visualizar os dados
ggplot(pib, aes(x = date, y = pib)) + 
  geom_line(size = .8) + 
  scale_x_yearqtr(breaks = pretty_breaks(n = 4), format = '%Y%q')

ggplot(pib, aes(x = date, y = var_interanual)) + 
  geom_line(size = .8) + 
  scale_x_yearqtr(breaks = pretty_breaks(n = 4), format = '%Y%q')

ggplot(pib_sa, aes(x = date, y = pib_sa)) + 
  geom_line(size = .8) + 
  scale_x_yearqtr(breaks = pretty_breaks(n = 4), format = '%Y%q')

ggplot(pib_sa, aes(x = date, y = var_marginal)) + 
  geom_line(size = .8) + 
  scale_x_yearqtr(breaks = pretty_breaks(n = 4), format = '%Y%q')

data %>%
  gather(variavel, valor, -date) %>%
  filter(variavel != 'pib' & date > '2017 Q1') %>%
  ggplot(aes(x = date, y = valor, colour = variavel)) +
  geom_line(size = .8) + 
  facet_wrap(~variavel, scales = 'free') +
  theme(legend.position = 'none')

############################ EXERCICIO 02 ############################

url = 'https://www.anfavea.com.br/docs/SeriesTemporais_Autoveiculos.xlsm'
download.file(url, destfile = 'veiculos.xlsm', mode = 'wb')
data_veiculos = read_excel('veiculos.xlsm',
                           col_types = c('date',
                                         rep('numeric', 25)),
                           skip = 4)

data_veiculos = data_veiculos[!rowSums(data_veiculos[,-1] == 0),] %>%
  select(`...1`, `Produção...5`) %>%
  rename(date = "...1", veiculos = "Produção...5") %>%
  mutate(veiculos = veiculos/1000, margem = (veiculos/lag(veiculos, 1)-1)-100)

View(data_veiculos)
colnames(data_veiculos)

              
              
              