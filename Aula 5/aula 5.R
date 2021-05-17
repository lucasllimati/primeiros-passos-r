##########################################################
####### Visualização de dados com o pacote ggplot2 #######
##########################################################

getwd()
setwd('C:/Users/lucas.lima/Documents/Arquivos RStudio/Aula 5')
getwd()

## Carregar pacotes
library(tidyverse)
library(lubridate)
library(zoo)
install.packages("gridExtra")
library(gridExtra)

## Coleta de dados

covid_df = read_csv('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv', 
                    guess_max = 10000) %>%
  group_by(state) %>%
  mutate(MM_mortes = rollmean(newDeaths, k = 7, fill = NA, align = 'right'),
         MM_casos = rollmean(newCases, k = 7, fill = NA, align = 'right'),
         d_vaccinated = vaccinated - lag(vaccinated,1),
         MM_dose1 = rollmean(newCases, k = 7, fill = NA, align = 'right'))
View(covid_df)
colnames(covid_df)


## Visualização de dados

g1 = covid_df %>%
  filter(state == 'TOTAL') %>%
  ggplot(aes(x=date)) +
  geom_bar(aes(y = newDeaths), stat = 'identity', fill = 'lightblue', colour = 'lightblue', width = .00001) +
  geom_line(aes(y = MM_mortes), colour = 'red', size = .8) +
  labs(x = '', y = 'Novas mortes',
       title = 'Mortes por COVID-19 no Brasil',
       subtitle = 'Média Móvel de 7 dias',
       caption = 'Fonte dos dados: https://github.com/wcota/covid19br') + 
  theme(plot.title = element_text(size = 10, face = 'bold'),
        plot.subtitle = element_text(size = 8, face = 'italic'))
g1

g2 = covid_df %>%
  filter(state == 'TOTAL') %>%
  ggplot(aes(x=date)) +
  geom_bar(aes(y = newCases / 1000), stat = 'identity', fill = 'lightblue', colour = 'lightblue', width = .00001) +
  geom_line(aes(y = MM_casos / 1000), colour = 'red', size = .8) +
  labs(x = '', y = 'Novas Casos (mil)',
       title = 'Novos casos de COVID-19 no Brasil',
       subtitle = 'Média Móvel de 7 dias',
       caption = 'Fonte dos dados: https://github.com/wcota/covid19br') + 
  theme(plot.title = element_text(size = 10, face = 'bold'),
        plot.subtitle = element_text(size = 8, face = 'italic'))
g2

grid.arrange(g1 ,g2, ncol = 2)

## Visualização de dados 02
g1_suldeste = covid_df %>%
  filter(state %in% c('RJ', 'SP', 'MG', 'ES')) %>%
  ggplot(aes(x = date, y = MM_casos, colour = state)) +
  geom_line(size = .8) +
  facet_wrap(~state, scales = 'free') +
  theme(legend.position = 'none')
g1_suldeste

g2_suldeste = covid_df %>%
  filter(state %in% c('RJ', 'SP', 'MG', 'ES')) %>%
  ggplot(aes(x = date, y = MM_mortes, colour = state)) +
  geom_line(size = .8) +
  facet_wrap(~state, scales = 'free') +
  theme(legend.position = 'none')
g2_suldeste

## Visualização de dados 03
covid_df %>%
  filter(state == 'TOTAL' & date > '2021-01-17') %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = d_vaccinated / 1000), stat = 'identity',
           fill = '#8abbd0', colour = 'black') +
  geom_line(aes(y = MM_dose1 / 1000), colour = 'red', size = .8) +
  scale_x_date(breaks = '7 days',
               date_labels = '%d/%m') +
  labs(x = '', y = 'Doses diárias (mil)',
       title = 'Número de doses aplicadas no dia (1ª dose)')

## Visualização de dados 04 - todos os estados
covid_df %>%
  filter(state != 'TOTAL') %>%
  ggplot(aes(x = date, y = MM_casos, colour = state)) +
  geom_line(size = .8) +
  facet_wrap(~state, scales = 'free') +
  theme(legend.position = 'none')

covid_df %>%
  filter(state != 'TOTAL') %>%
  ggplot(aes(x = date, y = MM_mortes, colour = state)) +
  geom_line(size = .8) +
  facet_wrap(~state, scales = 'free') +
  theme(legend.position = 'none')

## Visualização de dados 05
covid_mensal_brasil = covid_df %>%
  select(date, state, newDeaths) %>%
  group_by(state, month = floor_date(date, 'month')) %>%
  summarise(newDeaths = sum(newDeaths)) %>%
  filter(state == 'TOTAL')
covid_mensal_brasil

covid_mensal_brasil %>%
  ggplot(aes(x = month, y = newDeaths / 1000)) +
  geom_bar(stat = 'identity', 
            fill = ifelse(covid_mensal_brasil$newDeaths > 60000,
                          "red", "#91b8bd"),
           colour = ifelse(covid_mensal_brasil$newDeaths > 60000,
                           "red", "#91b8bd")) +
geom_text(aes(label = newDeaths / 1000), size = 3, vjust = -0.5, fontface = 'bold') +
scale_x_date(breaks = '2 month', date_labels = '%b/%y') +
labs(x = '', y = '',
     title = 'Mortes mensais por COVID-19 no Brasil')
    
           
           