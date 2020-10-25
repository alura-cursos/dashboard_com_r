library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)

#install.packages("shinyWidgets")
library(shinyWidgets)

#### 5.2 Selecao de Datas(data range) ####

dados <- fread('dados_limpos.csv',encoding = 'UTF-8')

## front-end (tela que sera mostrada para o usuario)
ui = fluidPage(
  ## titulo da pagina
  titlePanel("Dashboard PROCON"),
  sidebarLayout(
    sidebarPanel(
      ## caixa de selecoo 
      checkboxGroupInput(inputId = "select_UF",label =  "Estado:",
                         choices = c('TODOS',unique(dados$UF)),selected = 'TODOS'),
      
      ## calendario para selecionar perado
      dateRangeInput(inputId = "data_abertura",label =  "Data Abertura:",
                     start = min(as.Date(dados$DataAbertura)),#"2001-01-01",
                     end   = max(as.Date(dados$DataAbertura))), #"2010-12-31"),
      
      ## selecoo de ano com outro tipo de filtro (atividade SABER MAIS)
      #sliderInput("unifRange", "Range",
      #           min = as.numeric(min(dados$anocalendario)), 
      #          max = as.numeric(max(dados$anocalendario)), 
      #         value = c(as.numeric(min(dados$anocalendario)), 
      #                  as.numeric(max(dados$anocalendario))),sep = ''),
      
    ),
    mainPanel(
      ## grafico de linhas
      plotlyOutput(outputId = 'data',width = '100%'),
      
      ## texto descritivo do grafico de linhas
      textOutput(outputId = "descData"),
      
      ## grafico
      plotlyOutput(outputId = 'uf'),
      ## texto descritivo do grafico
      textOutput(outputId = "descUf"),
      
      ## grafico
      plotlyOutput(outputId = 'atendida'),
      
      ## grafico
      plotlyOutput(outputId = 'atendidaAno'),
      
    )
  )
)

## back-end (o que o sistema ira executar para retornar para o usuario, front-end)
server = function(input, output, session) {
  
  dados_selecionados <- reactive({
    ## filtro UF
    print(input)
    if (!'TODOS' %in% input$select_UF  ){
      dados <- dados %>% filter(UF %in% input$select_UF)
    }
    ## filtro DATA
    dados <- dados %>% filter(as.Date(DataAbertura) >= input$data_abertura[1] & 
                                as.Date(DataAbertura) <= input$data_abertura[2])
    dados
    
  })
  
  ## grafico de linhas ano-mes
  output$data <- renderPlotly({
    ano_mes <- data.frame(table(format(as.Date(dados_selecionados()$DataAbertura),
                                       '%Y-%m'))) %>% rename(Data = Var1, Qtd=Freq)
    ano_mes$Data <- as.Date(paste(ano_mes$Data,'01',sep = '-'))
    
    ggplotly(
      ggplot(data = ano_mes, aes(Data, Qtd)) +
        geom_line(group = 1) +
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 45,hjust = 1))+
        ggtitle('Quantidade de Reclamacoes por Ano-Mes') +
        scale_x_date(date_labels = '%b-%Y',breaks = '6 month')
    )  
  })
  
  ## grafico UF
  output$uf   <- renderPlotly({ 
    ggplotly(
      data.frame(table(dados_selecionados()$UF)) %>% rename(UF = Var1,Qtd = Freq) %>%
        ggplot(aes(x = reorder(UF,Qtd),y = Qtd,
                   text=paste(" UF:", UF, "<br>", "QTD:",Qtd))) + 
        geom_bar(fill = 'blue',stat = 'identity') +  
        coord_flip() +
        xlab('UF') + #ylab('Quantidade') + 
        theme_bw() + 
        ggtitle('Quantidade de Reclamacoes por UF'),
      tooltip = "text"
    )
  })
  
  ## grafico atendida
  output$atendida    <- renderPlotly({ 
    ggplotly(
      ggplot(dados_selecionados()) +
        geom_bar(aes(Atendida),fill = c('red','green'),stat = 'count') +
        ylab('Quantidade') + 
        theme_bw() + 
        ggtitle('Quantidade de Chamados Atendidos')
    )  
  })
  
  ## grafico atendida por ano
  output$atendidaAno <- renderPlotly({ 
    ggplotly(
      data.frame(table(dados_selecionados()$anocalendario,dados_selecionados()$Atendida)) %>%
        rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>%
        ggplot() +
        geom_bar(aes(x = Ano,y = Qtd, fill = Atendida),
                 stat = 'identity',position = position_dodge2()) +
        theme_bw() + 
        ggtitle('Quantidade de Reclamacoeses Atendidas(nao) por Ano')
    )
  })
  
  ## retornando texto para cada campo em especifico
  output$descData <- renderText({
    paste("Grafico com a quantidade de reclamacoeses feitas entre:",
          min(dados_selecionados()$DataAbertura),'-',
          max(dados_selecionados()$DataAbertura))
  })
  output$descUf   <- renderText({
    estados <- paste(unique(dados_selecionados()$UF),collapse = ', ')
    paste("Grafico com a quantidade de reclamacoes feitas por UF: ",estados)
  })
  output$descAtendida    <- renderText({"Grafico com a quantidade de reclamacoes atendidas e nao atendidas"})
  
  output$descAtendidaAno <- renderText({"Grafico com a quantidade de reclamacoes atendidas e nao atendidas por Ano"})

}

shinyApp(ui, server)
#runApp(list(ui = ui, server = server),launch.browser = TRUE)
