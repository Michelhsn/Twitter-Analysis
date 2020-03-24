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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Analise de Comportamento"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            textInput("text", h3("Digite a Tag")),
            actionButton("go", "Pesquisar"),
            textInput("text1", h3("Digite o nome sem @")),
            actionButton("go1", "Pesquisar"),
            
        ),
        
       

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        ),
        
        
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    ntext <- eventReactive(input$go, {
        isolate({
            withProgress({
                setProgress(message = "Processando... Isso pode demorar alguns minutos")
                tweets <- search_tweets(input$text, n = 50)
                
                users <- unique(tweets$screen_name)
                
                data <- botornot(users, fast = TRUE)
                
                
                data$bot <- 1
                for (i in 1:nrow(data)){
                    
                    if (data$prob_bot[i] > 0.95)
                        data$bot[i] = "bot"
                    else
                        data$bot[i] = "humano"
                }
                
                df<-as.data.frame(data)
                
                
                ggplot(data = df) + geom_bar(mapping = aes(x = bot, y = ..prop.., group = 1), stat = "count", fill=c("purple", "steelblue")) + 
                    scale_y_continuous(labels = scales::percent_format()) + labs(x = "Comportamento", y = "Percentagem", title = input$text)
                
            })
        })

    })
    
    ntext <- eventReactive(input$go1, {
        isolate({
            withProgress({
                setProgress(message = "Processando... Isso pode demorar alguns minutos")
                
                
                
                # list bot accounts
                bots <- c('Estadao')
                users <- c(input$text1)
                
                data <- botornot(bots)
                
                    
                    if (data$prob_bot[1] > 0.95){
                        ggplot() + 
                            annotate("text", x = i, y = i, size=5, label = "bot",xmax = 15, xmin = 0 ) + 
                            theme_bw() +
                            theme(panel.grid.major=element_blank(),
                                  panel.grid.minor=element_blank())
                        
                    }else{
                        ggplot() + 
                            annotate("text", x = i, y = i, size=5, label = "humano",xmax = 15, xmin = 0 ) + 
                            theme_bw() +
                            theme(panel.grid.major=element_blank(),
                                  panel.grid.minor=element_blank())
                    }
                        
               
                
        
            })
        })
        
    })
    
    output$distPlot <- renderPlot({
        ntext()
    })
    
        
    
  
}

# Run the application 
shinyApp(ui = ui, server = server)
