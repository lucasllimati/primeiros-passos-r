##############################################
##### Como importar dados para o RStudio #####
##############################################

# Iniciando bibliotecas/pacotes
library(readr)
getwd()

## Importando arquivo .csv

# csv simples
heigths = read_csv('heights.csv')
heigths
View(heigths)

# csv filtrando linhas, nome das colunas e tipo dos dados
sidra = read_csv2('sidra.csv', skip = 5, 
                  col_names = 
                    c('mes', 'vendas', 'vendas_sa', 'receita', 'receita_sa'),
                  col_types = 
                    cols(
                      mes = col_date(format = '%d/%m/%Y'),
                      vendas = col_double(),
                      vendas_sa = col_double(),
                      receita = col_double(),
                      receita_sa = col_double()
                      ),
                  locale = locale(decimal_mark = ','))
sidra
View(sidra)

# csv com enconding LATIN1 -> caracteres brasileiros
subitem = read_csv2('subitem.csv',
                    locale = locale(encoding = 'Latin1'))
subitem
View(subitem)

# guess_parser -> determinar qual tipo de vetor é o "melhor".
?guess_parser
guess_parser('2010-10-01')
guess_parser('15:00')
guess_parser(c('TRUE', 'FALSE'))

## Importando arquivo .txt
doc.text = readLines('comunicado226.txt')

## Importando planilhas excel
library(readxl)
url = 'https://analisemacro.com.br/download/27398'
download.file(url, destfile = 'resultado.xlsx', mode='wb')
data_stn = read_excel('resultado.xlsx', 
                      sheet = '1.1', 
                      range = "B5:JV79", 
                      col_types = 'numeric')

## Importando arquivo zipados .zip
# Importando arquivo .zip
temp = tempfile()
download.file("http://www.bcb.gov.br/ftp/notaecon/Divggnp.zip", temp)
unzip(temp, list = TRUE)
datazip = unzip(temp, files = 'Divggnp.xls')

# Importando arquivo .gz
# Installing 'vroom'
library(vroom)
dados_covid = vroom::vroom('https://data.brasil.io/dataset/covid19/caso_full.csv.gz')
