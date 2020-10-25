library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(shinydashboard)

#### 7.2. Alterando cores página . Inserindo conteudo na app  ####


dados <- fread('dados_limpos.csv',encoding = 'UTF-8')
media_chamados_ano <- dados %>%
  group_by(anocalendario) %>%
  summarise(qtd_chamados = n()) %>%
  summarise(media_chamados_ano = mean(qtd_chamados)) %>%
  as.numeric()


cabecalho     <- dashboardHeader(title = "Dashboard PROCONs",
                                 titleWidth = '250px')
barra_lateral <- dashboardSidebar(width = '250px',
                  sidebarMenu(
                    menuItem('Dashboard',tabName = 'dashboard',
                             icon = icon('dashboard')),
                    menuItem('Informações',tabName = 'infos',
                             icon = icon('info-circle'))
                  )
                 )
painel_principal  <- dashboardBody(
                  tabItems(
                    tabItem(tabName = "infos",
                        h1("Informações"),
                        infoBox(title = 'Contato',icon = icon('envelope-square'),
                                subtitle = 'Para mais informações e/ou feedback
                                entre em contato em: gabriel.lg08@gmail.com')
                    ),
                    tabItem(tabName = "dashboard",
                            fluidRow(
                              # valueBox com qtd de registros da base inicial ('estático')
                              valueBox(subtitle =  "Registros",value = nrow(dados), 
                                       icon = icon("database"),color = 'blue'),
                              
                              # infobox com média ('estático')
                              infoBox(title =  '',subtitle  = "Reclamações por Ano",
                                      value = media_chamados_ano,
                                      icon = icon("list")),
                              
                              
                              #valueBox com valores vindo do server (dinamico)
                              valueBoxOutput("approvalBox")
                              
                            ),
                            
                            fluidRow(
                              column(width = 12,
                                     box(title = "FILTROS",width = '100%',
                                         solidHeader = T,
                                         collapsible = T,
                                         status = 'warning',
                                         
                                         column(width = 12,
                                                box(width = '100%',
                                                    ## caixa de seleção 
                                                    awesomeCheckboxGroup(inputId = "select_UF",
                                                                         label =  "Estado:",inline = TRUE,
                                                                         choices = c('TODOS',sort(unique(dados$UF))),
                                                                         selected = 'TODOS')
                                                )
                                         ),
                                         column(width = 6,
                                                box(width = '60%',
                                                    ## calendário para selecionar período
                                                    dateRangeInput(inputId = "data_abertura",label =  "Data Abertura:",
                                                                   start = min(as.Date(dados$DataAbertura)),#"2001-01-01",
                                                                   end   = max(as.Date(dados$DataAbertura))), #"2010-12-31"),
                                                )
                                         ),
                                         column(width = 6,
                                                box(width = '60%',
                                                    ## seleção de descrição de assunto
                                                    selectizeInput(inputId = "assunto",label =  "Descrição Assunto:",
                                                                   choices = c('TODOS', unique(dados$DescricaoAssunto)),
                                                                   selected = 'TODOS',multiple = T,options = list(maxItems = 5))
                                                )
                                         )
                                     )## FIM BOX
                              ), ## FIM COLUMNS
                            ),
                            fluidRow(
                              column(width = 12,
                                     box(title = "Quantidade de Reclamações por Ano-Mês",width = '100%',
                                         background = 'navy',
                                         ## gráfico de linhas
                                         plotlyOutput(outputId = 'data',width = '100%'),
                                         ## texto descritivo do grafico de linhas
                                         textOutput(outputId = "descData")
                                     )
                              ) # FIM COLUNA(COLUMN)
                            ),# FIM LINHA(ROW)
                            fluidRow(
                              column(width = 6,
                                     box(title = "Quantidade de Reclamacoes Atendidas",width = '100%',
                                         status = 'primary',solidHeader = T,
                                         ## gráfico
                                         plotlyOutput(outputId = 'atendida'),
                                         ## texto descritivo do gráfico
                                         textOutput(outputId = "descAtendida"),
                                     )
                              ),
                              column(width = 6,
                                     box(title = 'Quantidade de Reclamações (Não)Atendidas por ANO',width = '100%',
                                         status = 'primary',solidHeader = T,
                                         ## gráfico
                                         plotlyOutput(outputId = 'atendidaAno'),
                                         ## texto descritivo do gráfico
                                         textOutput(outputId = "descAtendidaAno"),
                                     )
                              )## FIM COLUNA
                            ), ## FIM LINHA
                            fluidRow(
                              column(width = 12,
                                     box(title = "Reclamações por UF",width = '100%',
                                         status = 'danger',
                                         ## gráfico
                                         plotlyOutput(outputId = 'uf'),
                                         ## texto descritivo do gráfico
                                         textOutput(outputId = "descUf")
                                     )
                              )
                            )
                    )
                  )
)

ui = dashboardPage(header  = cabecalho,
                   sidebar = barra_lateral,
                   body    = painel_principal,
                   skin = 'green')


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
        #ggtitle('Quantidade de Reclamações por Ano-Mês') +
        scale_x_date(date_labels = '%b-%Y',breaks = '6 month')
    )  
  })
  
  ## gráfico UF
  output$uf   <- renderPlotly({ 
    ggplotly(
      data.frame(table(dados_selecionados()$UF)) %>% rename(UF = Var1,Qtd = Freq) %>%
        ggplot(aes(x = reorder(UF,Qtd),y = Qtd,
                   text=paste(" UF:", UF, "<br>", "QTD:",Qtd))) + 
        geom_bar(fill = 'blue',stat = 'identity') +  
        coord_flip() +
        xlab('UF') + #ylab('Quantidade') + 
        theme_bw(), 
        #ggtitle('Quantidade de Reclamações por UF'),
      tooltip = "text"
    )
  })
  
  ## gráfico atendida
  output$atendida    <- renderPlotly({ 
    ggplotly(
      ggplot(dados_selecionados()) +
        geom_bar(aes(Atendida),fill = c('red','green'),stat = 'count') +
        ylab('Quantidade') + 
        theme_bw() #+ ggtitle('Quantidade de Chmados Atendidos')
    )  
  })
  
  ## gráfico atendida por ano
  output$atendidaAno <- renderPlotly({ 
    ggplotly(
      data.frame(table(dados_selecionados()$anocalendario,
                       dados_selecionados()$Atendida)) %>%
        rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>%
        ggplot() +
        geom_bar(aes(x = Ano,y = Qtd, fill = Atendida),
                 stat = 'identity',position = position_dodge2()) +
        theme_bw() #+ ggtitle('Quantidade de Reclamações Atendidas(não) por Ano')
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
  
  output$approvalBox <- renderValueBox({
    valueBox(value = length(unique(dados_selecionados()$UF)), 
             subtitle = "Estados Selecionados", 
             icon = icon("map-marker"),color = 'blue' )
  })
  
}


shinyApp(ui, server)
