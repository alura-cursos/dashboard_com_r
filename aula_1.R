getwd()
setwd('D:/Projetos/alura/r_shiny')


library(data.table)
library(dplyr)

#### 1. preparando os dados ####
dados <- fread('reclamacao.csv',encoding = 'UTF-8')

## verificando tipos/valores todas as colunas
summary(dados)

## nome das colunas
colnames(dados)

## excluindo colunas irrelevantes para a base
reclamacao <- dados %>%
                select(-X.1, -V1)

## verificando anos
#table(reclamacao$anocalendario)

#excluindo registros invalidas na coluna região
unique(reclamacao$regiao)
reclamacao <- reclamacao %>%
                filter(regiao != 'N/D')

#unique(reclamacao$UF)
#unique(reclamacao$Tipo)

unique(reclamacao$Atendida)
reclamacao$Atendida <- gsub(pattern = 'S|Siim',replacement = 'sim',x = reclamacao$Atendida)
reclamacao$Atendida <- gsub(pattern = 'N|nAoo|nao',replacement = 'não',x = reclamacao$Atendida)
reclamacao          <- reclamacao %>% filter(Atendida != '')
unique(reclamacao$Atendida)

unique(reclamacao$SexoConsumidor)  
reclamacao$SexoConsumidor <- gsub(pattern = 'N|NULL',replacement = 'N/I',
                                  x = reclamacao$SexoConsumidor)
reclamacao <- reclamacao %>% 
                filter(!SexoConsumidor %in% c(''))
unique(reclamacao$SexoConsumidor)

## salvando os dados limpos/preparados
fwrite(reclamacao,'dados_limpos.csv',row.names = F)

