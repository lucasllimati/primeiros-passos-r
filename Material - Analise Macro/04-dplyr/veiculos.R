########################################################
########### Análise da Produção de Veículos ############


library(tidyverse)
library(readxl)
library(tstools)
library(scales)
library(knitr)


## Fazer download dos dados
url = 'http://www.anfavea.com.br/docs/SeriesTemporais_Autoveiculos.xlsm'
download.file(url, destfile = 'veiculos.xlsm', mode='wb')
data = read_excel('veiculos.xlsm', col_types = c('date', 
                                                 rep('numeric', 25)),
                  skip=4)

## Tratamento dos dados
data = data[!rowSums(data[,-1])==0,] %>%
  dplyr::select(`...1`, `Produção...5`) %>%
  rename(date = "...1", veiculos = "Produção...5") %>%
  mutate(veiculos = veiculos/1000) %>%
  mutate(margem = (veiculos/lag(veiculos,1)-1)*100) %>%
  mutate(interanual = (veiculos/lag(veiculos,12)-1)*100) %>%
  mutate(anual = acum_i(veiculos,12))

data_long = 
  data %>%
  gather(variavel, valor, -date)

## Visualização dos dados
data %>%
  tail() %>%
  kable(digits=2,
        caption='Produção de Veículos', format='html')


ggplot(data, aes(x=date, y=veiculos))+
  geom_line()+
  scale_x_datetime(breaks = date_breaks('4 year'),
                   labels = date_format("%Y"))+
  labs(x='', y='',
       title='Produção de Veículos (mil)')

data_long %>%
  filter(date > '2014-01-01') %>%
  ggplot(aes(x=date, y=valor, colour=variavel))+
  geom_line(size=.8)+
  facet_wrap(~variavel, scales='free')+
  theme(legend.position = 'none',
        strip.text = element_text(size=10, face='bold'))+
  labs(x='', y='',
       title='Produção de Veículos')
