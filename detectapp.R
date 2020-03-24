#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(rtweet)
library(ggplot2)
library(httpuv)
library(tweetbotornot)

if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}

# install remotes package if it's not already


## install dev version of rtweet from github


## load rtweet package
library(rtweet)

## store api keys (these are fake example values; replace with your own keys)
api_key <- "1roTr9unhjhU1c3Yk1RRyn9sN"
api_secret_key <- "apHe9exUDv52pGvelaAa5kzB88X7Vev9rtmBLOiM1kEfwWXb9a"



## store api keys (these are fake example values; replace with your own keys)
api_key <- "1roTr9unhjhU1c3Yk1RRyn9sN"
api_secret_key <- "apHe9exUDv52pGvelaAa5kzB88X7Vev9rtmBLOiM1kEfwWXb9a"
access_token <- "2507354983-KRq6MTyKY7hyOtgAIGgq4stW4FYMkdOaNsT9fjX"
access_token_secret <- "P6d3pokpFhmfePsxQOVhDsTEJITo2C0T38jSWymCmg3po"

## authenticate via web browser
token <- create_token(
  app = "AccessAPITry",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Análise de Comportamento"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            textInput("text", h3("Digite a Tag")),
            actionButton("go", "Pesquisar"),
            textInput("text1", h3("Digite o nome sem @")),
            actionButton("go1", "Pesquisar")
            
        ),
        
       

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           verbatimTextOutput("textUser")
        )
        
        
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    ntext <- eventReactive(input$go, {
        isolate({
            withProgress({
                setProgress(message = "Processando... Isso pode demorar alguns minutos")
                tweets <- search_tweets(input$text, n = 150)
                
                users <- unique(tweets$screen_name)
                
                data <- botornot(users)
               
                
                data$bot <- 1
                
                for (i in 1:nrow(data)){
                    
                    if (data$prob_bot[i] > 0.95)
                        data$bot[i] = "bot"
                    else
                        data$bot[i] = "humano"
                }
                
                output$textUser <- renderText({
                  usuariosbots <- c("Usuários com comportamento bot nesta tag: ")
                  for (i in 1:nrow(data)){
                    
                    if (data$prob_bot[i] > 0.98){
                      usuariosbots <- append(usuariosbots, c(data$screen_name[i]))
                      
                    } else {
                      
                    }
                      
                  }
                  usuariosbots
                })
                
                usuariosbots <- c("")
                
                df<-as.data.frame(data)
                
                
                ggplot(data = df) + geom_bar(mapping = aes(x = bot, y = ..prop.., group = 1), stat = "count", fill=c("purple", "steelblue")) + 
                    scale_y_continuous(labels = scales::percent_format()) + labs(x = "Comportamento", y = "Percentagem", title = input$text)
                
            })
        })

    })
    
    
    output$distPlot <- renderPlot({
        ntext()
    })
    
    
    
    
    observeEvent(input$go1, {
      users <- c(input$text1)
      
      data <- botornot(users)
      
      if (data$prob_bot >0.95){
        texto <- "Este perfil possui comportamento bot"
      } else {
        texto <- "Este perfil não possui comportamento bot"
      }
      showModal(modalDialog(
        title = "Resultado",
        texto,
        easyClose = TRUE
      ))
      
      
      
    })
    
    
    
        
    
  
}

# Run the application 
shinyApp(ui = ui, server = server)
