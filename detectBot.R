

library(rtweet)
library(dplyr)
library(tweetbotornot)
library(openssl)
library(ggplot2)
library(hrbrthemes)
library(scales)

## install remotes if not already
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

## install tweetbotornot from github
devtools::install_github("Michelhsn/tweetbotornot")

# gather tweets
tweets <- search_tweets("#PYONGEXPULSO", n = 10)

users <- unique(tweets$screen_name)
users

# get bot probability estimates
data <- botornot(users)


hist(data$prob_bot, main = "#MaiaGolpista", xlab = "Probabilidade de bot", ylab = "Frequência")
par(mar = c(5, 4, 4, 2) + 0.1)
# hash the usernames
data$user_hash <- md5(data$user_id)




data$bot <- funcbot(data$prob_bot)

for (i in 1:nrow(data)){
  
  if (data$prob_bot[i] > 0.95)
  data$bot[i] = "bot"
  else
    data$bot[i] = "humano"
}

# arrange by prob ests
data %>% 
  arrange(desc(prob_bot)) %>% 
  select()

a <- data[order(-data$prob_bot), ]

newdata <- a[1:10,]

barplot(table(data$bot),
        xlab = "Prob bot",
        ylab = "Frequência", ylim = c(0,150))

df<-as.data.frame(data)


library(ggplot2)
ggplot(data = df) + geom_bar(mapping = aes(x = bot, y = ..prop.., group = 1), stat = "count", fill=c("purple", "steelblue")) + 
  scale_y_continuous(labels = scales::percent_format()) + labs(x = "Comportamento", y = "Percentagem", title = "#MaiaGolpista")





## Criando um dataframe separadado a cada 10%
prob_de_ser <- as.data.frame(table(cut(data$prob_bot, 10))) 

## Inserindo o nome das colunas
prob_de_ser$Var1 <- c('De 0 a 10%', 'De 10 a 20%', 'De 20 a 30%', 'De 30 a 40%', 'De 40 a 50%',
                      'De 50 a 60%', 'De 60 a 70%', 'De 70 a 80%', 'De 80 a 90%', 'De 90 a 100%')

## Criando o gráfico
plot_ly(prob_de_ser,x = ~Var1,y = ~Freq, type = 'bar',
        marker = list(color = c('rgba(0,0,255,1)', 'rgba(0,0,255,1)',
                                'rgba(0,0,255,1)', 'rgba(0,0,255,1)',
                                'rgba(0,0,255,1)', 'rgba(255,0,0,1)'
                                , 'rgba(255,0,0,1)', 'rgba(255,0,0,1)'
                                , 'rgba(255,0,0,1)', 'rgba(255,0,0,1)'))) %>%
  layout(title = "Probabilidade de ser um robô",
         xaxis = list(title = ""),
         yaxis = list(title = ""))



x=newdata$screen_name
y=newdata$prob_bot
names(y)=x

plot(y, xaxt="n")
axis(2, at=1:10, labels = names(y))

library(ggplot2)

for (i in 1:10){
  

text = newdata$screen_name[i]
ggplot() + 
  annotate("text", x = i, y = i, size=5, label = text,xmax = 15, xmin = 0 ) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

}

par(mar = c(0,0,0,0))
plot(c(0, 15), c(0, 15), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')

for (i in 1:10){
  text(x = i+1, y = i+1, newdata$screen_name[i], 
       cex = 1.5, col = "black", family="serif", font=2, adj=0.5)
}





# list bot accounts
bots <- c('Estadao')

# get botornot estimates
bot_data <- botornot(bots)

plot(table(bot_data))


class(data)

