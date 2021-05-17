###################################################
##### Análise do PIB com o R ######################

library(tidyverse)
library(sidrar)
library(zoo)
library(tstools)
library(scales)


## Coletar Números Indices do PIB
### Número Indice com ajuste sazonal

pib_sa = get_sidra(api='/t/1621/n1/all/v/all/p/all/c11255/90707/d/v584%202') %>%
  mutate(date = as.yearqtr(`Trimestre (Código)`, format='%Y%q')) %>%
  rename(pib_sa = Valor) %>%
  mutate(var_marginal = (pib_sa/lag(pib_sa,1)-1)*100) %>%
  select(date, pib_sa, var_marginal) %>%
  as_tibble()

### Número Índice sem ajuste 
pib = get_sidra(api='/t/1620/n1/all/v/all/p/all/c11255/90707/d/v583%202') %>%
  mutate(date = as.yearqtr(`Trimestre (Código)`, format='%Y%q')) %>%
  rename(pib = Valor) %>%
  mutate(var_interanual = (pib/lag(pib,4)-1)*100) %>%
  mutate(var_anual = acum_i(pib, 4)) %>%
  select(date, pib, var_interanual, var_anual) %>%
  as_tibble()

### Juntar os dados
df_pib = inner_join(pib_sa, pib, by='date') %>%
  drop_na()

### Transformar de wide para long

df_pib_long = 
  df_pib %>%
  gather(variavel, valor, -date)

### Visualizar os dados

filter(df_pib_long, variavel != 'pib' & 
         date > '2014 Q1') %>%
  ggplot(aes(x=date, y=valor, colour=variavel))+
  geom_line(size=.8)+
  facet_wrap(~variavel, scales='free')+
  theme(legend.position = 'none')+
  scale_x_yearqtr(breaks = pretty_breaks(n=4),
                  format='%YQ%q')+
  labs(x='', y='',
       title='A recuperação em V do PIB brasileiro',
       caption='Fonte: analisemacro.com.br com dados do IBGE')
