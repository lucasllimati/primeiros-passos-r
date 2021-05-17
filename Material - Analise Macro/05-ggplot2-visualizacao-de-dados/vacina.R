#################################################################
####### Dados de vacinação para Covid-19 no Brasil ##############
#################################################################


## Carregar pacotes
library(tidyverse)
library(lubridate)
library(zoo)
library(tsibble)
library(fable)
library(feasts)
library(tsibbledata)

## Coletar dados
covid_df = readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv",
                           guess_max = 10000) %>%
  group_by(state) %>%
  mutate(d_vaccinated = vaccinated - lag(vaccinated,1)) %>%
  mutate(MM_dose1 = rollmean(d_vaccinated, 7, NA, align='right'))

## Visualização dos dados
covid_df %>%
  filter(state == 'TOTAL' & date > '2021-01-17') %>%
  ggplot(aes(x=date))+
  geom_bar(aes(y=d_vaccinated/1000), stat='identity',
           fill='#8abbd0', colour='black')+
  geom_line(aes(y=MM_dose1/1000), colour='red', size=.8)+
  scale_x_date(breaks = '4 days',
                   date_labels = "%d/%m")+
  labs(x='', y='mil doses',
       title='Número de doses aplicadas no dia (1ª dose)',
       caption='Fonte dos dados: https://github.com/wcota/covid19br')+
  theme(axis.text.x = element_text(size=8, face='bold', angle=45, vjust=.5),
        axis.text.y = element_text(size=8, face='bold'),
        plot.title = element_text(size=11, face='bold'),
        plot.caption = element_text(face='bold'),
        panel.background = element_rect(fill='white',
                                        colour='white'),
        axis.line.x.bottom = element_line(colour='black',
                                          linetype = 'solid'),
        axis.line.y.left = element_line(colour='black',
                                        linetype = 'solid'))

covid_df %>%
  filter(date > '2021-01-07' & state %in% c('RJ', 'SP') &
           d_vaccinated > 0) %>%
  ggplot(aes(x=date, y=d_vaccinated, colour=state))+
  geom_bar(aes(y=d_vaccinated/1000), stat='identity',
           fill='#8abbd0', colour='black')+
  geom_line(aes(y=MM_dose1/1000), colour='red', size=.8)+
  scale_x_date(breaks = '15 days',
               date_labels = "%d/%m")+
  facet_wrap(~state, scales='free')+
  theme(axis.text.x = element_text(size=8, face='bold', angle=45, vjust=.5),
        axis.text.y = element_text(size=8, face='bold'),
        plot.title = element_text(size=11, face='bold'),
        plot.caption = element_text(face='bold'),
        panel.background = element_rect(fill='white',
                                        colour='white'),
        axis.line.x.bottom = element_line(colour='black',
                                          linetype = 'solid'),
        axis.line.y.left = element_line(colour='black',
                                        linetype = 'solid'))+
  labs(x='', y='', 
       title='Número de doses aplicadas no dia (1ª dose)',
       caption='Fonte dos dados: https://github.com/wcota/covid19br')

           


covid_mensal = covid_df %>%
  select(date, state, newDeaths) %>%
  group_by(state, month = floor_date(date, 'month')) %>%
  summarise(newDeaths = sum(newDeaths)) 

covid_mensal_brasil = covid_mensal %>%
  filter(state == 'TOTAL')
  
covid_mensal_brasil %>%
  ggplot(aes(x=month, y=newDeaths/1000))+
  geom_bar(stat='identity', fill=ifelse(covid_mensal_brasil$newDeaths>60000, 
                                        'red', '#91b8bd'),
           colour=ifelse(covid_mensal_brasil$newDeaths>60000, 
                         'red', '#91b8bd'))+
  geom_text(aes(label = newDeaths/1000), size=3,
            vjust=-0.5, fontface='bold')+
  scale_x_date(breaks = '1 month',
               date_labels = '%b/%y',
               expand = c(0,13))+
  theme(axis.text.x = element_text(size=7, face='bold'),
        axis.text.y = element_text(size=8, face='bold'),
        plot.title = element_text(size=11, face='bold'),
        plot.caption = element_text(face='bold'),
        panel.background = element_rect(fill='white',
                                        colour='white'),
        axis.line.x.bottom = element_line(colour='black',
                                          linetype = 'solid'),
        axis.line.y.left = element_line(colour='black',
                                          linetype = 'solid'))+
  labs(x='', y='',
       title='Mortes mensais por Covid-19 no Brasil (mil pessoas)',
       caption='Fonte: analisemacro.com.br')

covid_mensal %>%
  filter(state %in% c("RJ", "SP", "MG", 'ES')) %>%
  ggplot(aes(x=month, y=newDeaths, colour=state, fill=state))+
  geom_bar(stat='identity', width = 20)+
  facet_wrap(~state, scales='free')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=7, face='bold'))+
  scale_x_date(breaks = '3 month',
               date_labels = '%b/%y')+
  theme(axis.text.x = element_text(size=7, face='bold'),
        axis.text.y = element_text(size=8, face='bold'),
        plot.title = element_text(size=11, face='bold'),
        plot.caption = element_text(face='bold'),
        panel.background = element_rect(fill='white',
                                        colour='white'),
        axis.line.x.bottom = element_line(colour='black',
                                          linetype = 'solid'),
        axis.line.y.left = element_line(colour='black',
                                        linetype = 'solid'),
        strip.background = element_rect(fill='white', colour='white'),
        strip.text = element_text(face='bold'))+
  labs(x='', y='',
       title='Mortes mensais por Covid-19 no Sudeste',
       caption='Fonte: analisemacro.com.br')


covid_mensal %>%
  filter(state %in% c("RS", "SC", "PR")) %>%
  ggplot(aes(x=month, y=newDeaths, colour=state, fill=state))+
  geom_bar(stat='identity', width = 20)+
  facet_wrap(~state, scales='free')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=7, face='bold'))+
  scale_x_date(breaks = '4 month',
               date_labels = '%b/%y',
               expand = c(0,13))+
  theme(axis.text.x = element_text(size=7, face='bold'),
        axis.text.y = element_text(size=8, face='bold'),
        plot.title = element_text(size=11, face='bold'),
        plot.caption = element_text(face='bold'),
        panel.background = element_rect(fill='white',
                                        colour='white'),
        axis.line.x.bottom = element_line(colour='black',
                                          linetype = 'solid'),
        axis.line.y.left = element_line(colour='black',
                                        linetype = 'solid'),
        strip.background = element_rect(fill='white', colour='white'),
        strip.text = element_text(face='bold'))+
  labs(x='', y='',
       title='Mortes mensais por Covid-19 no Sul',
       caption='Fonte: analisemacro.com.br')

covid_mensal %>%
  filter(state != "TOTAL") %>%
  ggplot(aes(x=month, y=newDeaths, colour=state, fill=state))+
  geom_bar(stat='identity', width = 20)+
  facet_wrap(~state, scales='free')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=7, face='bold'))+
  scale_x_date(breaks = '3 month',
               date_labels = '%b/%y',
               expand = c(0,13))+
  theme(axis.text.x = element_text(size=7, face='bold'),
        axis.text.y = element_text(size=8, face='bold'),
        plot.title = element_text(size=11, face='bold'),
        plot.caption = element_text(face='bold'),
        panel.background = element_rect(fill='white',
                                        colour='white'),
        axis.line.x.bottom = element_line(colour='black',
                                          linetype = 'solid'),
        axis.line.y.left = element_line(colour='black',
                                        linetype = 'solid'),
        strip.background = element_rect(fill='white', colour='white'),
        strip.text = element_text(face='bold'))+
  labs(x='', y='',
       title='Mortes mensais por Covid-19 nos Estados',
       caption='Fonte: analisemacro.com.br')


covid_df %>%
  filter(state == 'RJ') %>%
  ggplot(aes(x=date, y=vaccinated))+
  geom_line()
