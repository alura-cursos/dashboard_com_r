
library(shiny)

#### 3. Criando app web com R ####

## exemplo de app shiny
#runExample('02_text')

## front-end (tela que será mostrada para o usuário)
ui <- fluidPage(
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
          verbatimTextOutput(outputId = "listaNumeros")
        )
      )
)

## back-end (o que o sistema irá executar para retornar para o usuário, front-end)
server <- function(input, output, session) {
  
  ## geração de uma lista de números que serão impressos na tela do usuário
  output$listaNumeros <-renderPrint({
    seq(1:input$bins)
  })
}

## função para execução o app web
shinyApp(ui, server)
