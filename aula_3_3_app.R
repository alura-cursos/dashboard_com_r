library(shiny)
library(data.table)
library(dplyr)

#### 3. Criando app web com R ####

## lendo arquivos 
dados <- fread('dados_limpos.csv')

## front-end (tela que será mostrada para o usuário)
ui = fluidPage(
  ## título da página
  titlePanel("Hello Shiny!"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      ## impressão dos dados gerados no 'server'
      verbatimTextOutput(outputId = "listaNumeros"),
      
      ## impressão dos dados gerados no 'server'
      verbatimTextOutput("listaUF")
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
  
  ## geração de uma lista de números que serão impressos na tela do usuário
  output$listaNumeros <-renderPrint({
    seq(1:input$bins)
  })
  
  ## retorna lisa de UF 
  output$listaUF <- renderPrint({
    unique(dados_selecionados()$UF)
  })
}

## executa o serviço web
shinyApp(ui, server)
