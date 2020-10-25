
#### 2. Construindo Gráficos interativos ####
library(plotly)
library(ggplot2)
library(data.table)

reclamacao <- fread('dados_limpos.csv')

### 2.1 barra vertical (Atendida) ####
grafico_atendida <- ggplot(reclamacao) +
  geom_bar(aes(Atendida),fill = c('red','green'),stat = 'count') +
  ylab('Quantidade') + 
  theme_bw() + 
  ggtitle('Quantidade de Chmados Atendidos')

## interatividade no gráfico
grafico_atendida <- ggplotly(grafico_atendida)
grafico_atendida

### 2.2 barra horizontal (UF) ####

## gráfico desordenado
#ggplot(reclamacao) + 
 # geom_bar(aes(UF),stat = 'count',) 

## gráfico ordenado
grafico_uf <- data.frame(table(reclamacao$UF)) %>% rename(UF = Var1,Qtd = Freq) %>%
  ggplot(aes(x = reorder(UF,Qtd),y = Qtd,
             text=paste(" UF:", UF, "<br>", "QTD:",Qtd))) + 
  geom_bar(fill = 'blue',stat = 'identity') +  
  coord_flip() +
  xlab('UF') + 
  theme_bw() + 
  ggtitle('Quantidade de Reclamações por UF')


grafico_uf <- ggplotly(grafico_uf,tooltip = "text")
grafico_uf

### 2.3 gráfico de linha (dataArq) ####

## gráfico de linhas data completa(visualizão ruim)
data.frame(table(as.Date(reclamacao$DataAbertura))) %>%
    rename(Data = Var1, Qtd=Freq) %>%
  ggplot(aes(Data,Qtd)) +
    geom_line(group = 1)

## gráfico de linhas data ano-mes
#ano_mes <- data.frame(table(format(as.Date(dados$DataArquivamento),
 #                                  '%Y-%m'))) %>% rename(Data = Var1, Qtd=Freq)
#ano_mes$Data <- as.Date(paste(ano_mes$Data,'01',sep = '-'))

grafico_data <- data.frame(table(as.Date(reclamacao$DataArquivamento))) %>%
                  rename(Data = Var1, Qtd=Freq) %>%
                ggplot(aes(as.Date(Data), Qtd)) +
                  geom_line(group = 1) +
                  theme_bw() + 
                  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
                  ggtitle('Quantidade de Reclamações por Ano-Mês') +
                  scale_x_date(date_labels = '%b-%Y',breaks = '6 month')

grafico_data <- ggplotly(grafico_data)
grafico_data

### 2.4 gráfico de barra 2 categorias (Atendida-ANO) ####

## tabela de frequência Ano-Atendida

## gráfico de barras com 2 variáveis
grafico_atendida_ano <- data.frame(table(reclamacao$anocalendario,reclamacao$Atendida)) %>%
  rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>%
  ggplot() +
  geom_bar(aes(x = Ano,y = Qtd, fill = Atendida),
           stat = 'identity',position = position_dodge2()) +
  theme_bw() + 
  ggtitle('Quantidade de Reclamações Atendidas(não) por Ano')


## interatividade no gráfico 
grafico_atendida_ano <- ggplotly(grafico_atendida_ano)
grafico_atendida_ano

