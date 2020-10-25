library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)

#### 4.1 Inserindo gráficos ####

## lendo arquivos 
dados <- fread('dados_limpos.csv')

## front-end (tela que será mostrada para o usuário)
ui = fluidPage(
  ## título da página
  titlePanel("Dashboard PROCON"),
  sidebarLayout(
    sidebarPanel(),
    
    # Show a plot of the generated distribution
    mainPanel(
      ## gráfico de linhas
      plotlyOutput(outputId = 'data'),
      
      ## gráfico
      plotlyOutput(outputId = 'uf'),
      
      ## gráfico
      plotlyOutput(outputId = 'atendida'),
      
      ## gráfico
      plotlyOutput(outputId = 'atendidaAno'),
    )
  )

)

## back-end (o que o sistema irá executar para retornar para o usuário, front-end)
server = function(input, output, session) {
  ## manipulando/filtrando dados 
  dados_selecionados <- reactive({
    dados %>% 
      filter(UF %in% c('DF','SC','GO'))
  })
  
  ## inserindo gráficos na interface web
  
  ##grafico data
  output$data <- renderPlotly({ 
    ano_mes <- data.frame(table(format(as.Date(dados_selecionados()$DataAbertura),
                                       '%Y-%m'))) %>% rename(Data = Var1, Qtd=Freq)
    ano_mes$Data <- as.Date(paste(ano_mes$Data,'01',sep = '-'))
    
    ggplotly(
      ggplot(data = ano_mes, aes(Data, Qtd)) +
        geom_line(group = 1) +
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 45,hjust = 1))+
        ggtitle('Quantidade de Reclamações por Ano-Mês') +
        scale_x_date(date_labels = '%b-%Y',breaks = '6 month')
    )
    
  }) ## fim grafico data
  
  ## gráfico UF
  output$uf   <- renderPlotly({ 
    data.frame(table(dados_selecionados()$UF)) %>% 
        rename(UF = Var1,Qtd = Freq) %>%
      ggplot(aes(x = reorder(UF,Qtd),y = Qtd,
                 text=paste(" UF:", UF, "<br>", "QTD:",Qtd))) + 
      geom_bar(fill = 'blue',stat = 'identity') +  
      coord_flip() +
      xlab('UF') + #ylab('Quantidade') + 
      theme_bw() + 
      ggtitle('Quantidade de Reclamações por UF')
      
  })## fim gráfico UF
  
  ## grafico atendida
  output$atendida    <- renderPlotly({ 
    ggplotly(ggplot(dados_selecionados()) +
      geom_bar(aes(Atendida),fill = c('red','green'),stat = 'count') +
      ylab('Quantidade') + 
      theme_bw() + 
      ggtitle('Quantidade de Chmados Atendidos'))
    
  }) ## fim grafico atendida
  
  ## grafico atendida ano
  output$atendidaAno <- renderPlotly({ 
    ggplotly( data.frame(table(dados_selecionados()$anocalendario,
                               dados_selecionados()$Atendida)) %>%
      rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>%
      ggplot() +
      geom_bar(aes(x = Ano,y = Qtd, fill = Atendida),
               stat = 'identity',position = position_dodge2()) +
      theme_bw() + 
      ggtitle('Quantidade de Reclamações Atendidas(não) por Ano'))
  
  })## fim grafico atendida ano
}


shinyApp(ui, server)
