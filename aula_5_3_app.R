library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)

#install.packages("shinyWidgets")
library(shinyWidgets)

#### 5.3 Multi seleção (select input)  ####

dados <- fread('dados_limpos.csv',encoding = 'UTF-8')

## front-end (tela que será mostrada para o usuário)
ui = fluidPage(
  ## título da página
  titlePanel("Dashboard PROCON"),
  sidebarLayout(
    sidebarPanel(
      ## caixa de seleção 
      checkboxGroupInput(inputId = "select_UF",label =  "Estado:",
                         choices = c('TODOS',unique(dados$UF)),selected = 'TODOS'),
      
      ## calendário para selecionar perído
      dateRangeInput(inputId = "data_abertura",label =  "Data Abertura:",
                     start = min(as.Date(dados$DataAbertura)),#"2001-01-01",
                     end   = max(as.Date(dados$DataAbertura))), #"2010-12-31"),
      
      ## seleção de descriçãoo de assunto
      selectizeInput(inputId = "assunto",label =  "Descrição Assunto:",
                     choices = c('TODOS', unique(dados$DescricaoAssunto)),
                     selected = 'TODOS',multiple = T,options = list(maxItems = 5)),
      
      ## seleção de ano (SABER MAIS)
      #sliderInput("unifRange", "Range",
      #           min = as.numeric(min(dados$anocalendario)), 
      #          max = as.numeric(max(dados$anocalendario)), 
      #         value = c(as.numeric(min(dados$anocalendario)), 
      #                  as.numeric(max(dados$anocalendario))),sep = ''),
    ),
    mainPanel(
      ## gráfico de linhas
      plotlyOutput(outputId = 'data',width = '100%'),
      
      ## texto descritivo do gráfico de linhas
      textOutput(outputId = "descData"),
      
      ## gráfico
      plotlyOutput(outputId = 'uf'),
      ## texto descritivo do gráfico
      textOutput(outputId = "descUf"),
      
      ## gráfico
      plotlyOutput(outputId = 'atendida'),
      
      ## gráfico
      plotlyOutput(outputId = 'atendidaAno'),
      
    )
  )
)

## back-end (o que o sistema irá executar para retornar para o usuário, front-end)
server = function(input, output, session) {
  
  dados_selecionados <- reactive({
    ## filtro UF
    if (!'TODOS' %in% input$select_UF  ){
      dados <- dados %>% filter(UF %in% input$select_UF)
    }
    ## filtro ASSUNTO
    if(!'TODOS' %in% input$assunto){
      dados <- dados %>% filter(DescricaoAssunto %in% input$assunto)
    }
    
    ## filtro DATA
    dados <- dados %>% filter(as.Date(DataAbertura) >= input$data_abertura[1] & 
                                as.Date(DataAbertura) <= input$data_abertura[2])
    dados
    
  })
  
  ## gráfico de linhas ano-mes
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
  })
  
  ## gráfico UF
  output$uf   <- renderPlotly({ 
    ggplotly(
      data.frame(table(dados$UF)) %>% rename(UF = Var1,Qtd = Freq) %>%
        ggplot(aes(x = reorder(UF,Qtd),y = Qtd,
                   text=paste(" UF:", UF, "<br>", "QTD:",Qtd))) + 
        geom_bar(fill = 'blue',stat = 'identity') +  
        coord_flip() +
        xlab('UF') + #ylab('Quantidade') + 
        theme_bw() + 
        ggtitle('Quantidade de Reclamações por UF'),
      tooltip = "text"
    )
  })
  
  ## gráfico atendida
  output$atendida    <- renderPlotly({ 
    ggplotly(
      ggplot(dados) +
        geom_bar(aes(Atendida),fill = c('red','green'),stat = 'count') +
        ylab('Quantidade') + 
        theme_bw() + 
        ggtitle('Quantidade de Chmados Atendidos')
    )  
  })
  
  ## gráfico atendida por ano
  output$atendidaAno <- renderPlotly({ 
    ggplotly(
      data.frame(table(dados$anocalendario,dados$Atendida)) %>%
        rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>%
        ggplot() +
        geom_bar(aes(x = Ano,y = Qtd, fill = Atendida),
                 stat = 'identity',position = position_dodge2()) +
        theme_bw() + 
        ggtitle('Quantidade de Reclamações Atendidas(não) por Ano')
    )
  })
  
  ## retornando texto para cada campo em específico
  output$descData <- renderText({
    paste("Gráfico com a quantidade de reclamações feitas entre:",
          min(dados_selecionados()$DataAbertura),'-',
          max(dados_selecionados()$DataAbertura))
  })
  output$descUf   <- renderText({
    estados <- paste(unique(dados_selecionados()$UF),collapse = ', ')
    paste("Gráfico com a quantidade de reclamações feitas por UF: ",estados)
  })
  output$descAtendida    <- renderText({"Gráfico com a quantidade de reclamações atendidas e não atendidas"})
  
  output$descAtendidaAno <- renderText({"Gráfico com a quantidade de reclamações atendidas e não atendidas por Ano"})

}

#shinyApp(ui, server)
runApp(list(ui = ui, server = server),launch.browser = TRUE)
