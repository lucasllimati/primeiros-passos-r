#########################################################
########## Análise do PIB com o R #######################

library(tidyverse)
library(lubridate)
library(tstools)
library(sidrar)
library(zoo)
library(scales)
library(gridExtra)
library(tsibble)
library(timetk)

## Dados sem ajuste sazonal
tabela = get_sidra(api='/t/1620/n1/all/v/all/p/all/c11255/90687,90691,90696,90707,93404,93405,93406,93407,93408/d/v583%202') %>%
  mutate(date = as.yearqtr(`Trimestre (Código)`, format='%Y%q')) %>%
  select(date, `Setores e subsetores`, Valor) %>%
  spread(`Setores e subsetores`, Valor) %>%
  rename(`Consumo do Governo` = `Despesa de consumo da administração pública`) %>%
  as_tibble()

## Dados com ajuste sazonal
tabela_sa = get_sidra(api='/t/1621/n1/all/v/all/p/all/c11255/90687,90691,90696,90707,93404,93405,93406,93407,93408/d/v584%202') %>%
  mutate(date = as.yearqtr(`Trimestre (Código)`, format='%Y%q')) %>%
  select(date, `Setores e subsetores`, Valor) %>%
  spread(`Setores e subsetores`, Valor) %>%
  rename(`Consumo do Governo` = `Despesa de consumo da administração pública`) %>%
  as_tibble()

## Variação na Margem
tabela_sa_ts = ts(tabela_sa[,-1], start=c(year(tabela_sa$date[1]), quarter(tabela_sa$date[1])), freq=4)
margem = (tabela_sa_ts/stats::lag(tabela_sa_ts,-1)-1)*100 
colnames(margem) <- colnames(tabela_sa[,-1])
margem = tk_tbl(margem, preserve_index = TRUE, rename_index = 'date') %>%
  gather(variavel, valor, -date)

## Variação Interanual
tabela_ts = ts(tabela[,-1], start=c(year(tabela$date[1]), quarter(tabela$date[1])), freq=4)
interanual = (tabela_ts/stats::lag(tabela_ts,-4)-1)*100 
colnames(interanual) <- colnames(tabela[,-1])
interanual = tk_tbl(interanual, preserve_index = TRUE, rename_index = 'date') %>%
  gather(variavel, valor, -date)


## Variação acumulada em quatro trimestres
anual = acum_i(tabela_ts,4) %>%
  as_tibble() %>%
  mutate(date = tabela$date) %>%
  drop_na() %>%
  select(date, everything()) %>%
  gather(variavel, valor, -date)

## Visualização dos dados
filter(interanual, date > '2014 Q1') %>%  
ggplot(aes(x=date, y=valor, colour=variavel))+
  geom_bar(stat='identity')+
  geom_hline(yintercept=0, colour='black', linetype='dashed')+
  facet_wrap(~variavel, scales = 'free')+
  theme(legend.position = 'none',
        strip.text = element_text(size=7, face='bold'),
        axis.text.x = element_text(size=6),
        plot.title = element_text(size=10, face='bold'),
        plot.subtitle = element_text(size=8, face='italic'))+
  scale_x_yearqtr(breaks = pretty_breaks(n=4),
                  format = "%YQ%q")+
  labs(x='', y='',
       title='Componentes do PIB',
       subtitle = 'Variação contra o mesmo trimestre do ano anterior (%)',
       caption='Fonte: analisemacro.com.br com dados do IBGE')
