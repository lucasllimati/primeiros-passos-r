#######################################################
############ Usando pipes no R ########################


library(tidyverse)
library(babynames)
library(rbcb)
library(lubridate)
library(latex2exp)


## Exemplo 01

diamonds = ggplot2::diamonds
diamonds2 = mutate(diamonds, price_per_carat = price/carat)

diamonds2 <- diamonds %>%
  mutate(price_per_carat = price/carat)

## Exemplo 02

babynames %>%
  filter(name %>% substr(1,3) %>% magrittr::equals('Ste')) %>%
  group_by(year, sex) %>%
  summarise(total = sum(n)) %>%
  qplot(year, total, color=sex, data = ., geom='line') %>%
  magrittr::add(ggtitle('Nomes que começam com Ste')) %>%
  print

## Exemplo 03

expectativa = get_twelve_months_inflation_expectations('IPCA',
                                                       start_date = '2006-01-01') %>%
  filter(smoothed == "S" & base == 0) %>%
  dplyr::select(date, mean, min, max) %>%
  group_by(mes = floor_date(date, 'month')) %>%
  summarise(mean = mean(mean),
            min = mean(min),
            max = mean(max)) %>%
  rename(date = mes) %>%
  mutate(exp_mean_12mesesantes = dplyr::lag(mean, 12),
         exp_min_12mesesantes = dplyr::lag(min,12),
         exp_max_12mesesantes = dplyr::lag(max,12)) %>%
  select(date, exp_mean_12mesesantes, exp_min_12mesesantes, exp_max_12mesesantes)

ipca = get_series('13522', start_date = '2007-01-01') %>%
  rename(inflacao = `13522`)

data = inner_join(ipca, expectativa, by='date')

data %>%
  filter(date > '2014-01-01') %>%
  ggplot(aes(x=date))+
  geom_ribbon(aes(ymin = exp_min_12mesesantes,
                  ymax = exp_max_12mesesantes,
                  colour='Intervalo de Expectativas',
                  fill='Intervalo de Expectativas'))+
  geom_line(aes(y=inflacao, colour='Inflação efetiva'))+
  geom_line(aes(y=exp_mean_12mesesantes, colour='Expectativa 12 meses antes'))+
  scale_colour_manual('',
                      values = c('Intervalo de Expectativas' = 'lightblue',
                                 'Inflação efetiva' = 'black',
                                 'Expectativa 12 meses antes' = 'red'))+
  scale_fill_manual('', values = c('Intervalo de Expectativas' = 'lightblue',
                                   'Inflação efetiva' = 'white',
                                   'Expectativa 12 meses antes' = 'white'))
