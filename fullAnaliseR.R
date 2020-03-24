library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(rtweet)
library(wordcloud)
library(wordcloud2)
library(tm)
library(RColorBrewer)
library(cluster)
library(fpc)
library(dplyr)
library(tidyr)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(rjson)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(gganimate)
library(lubridate)
library(maps)
library(ggthemes)
library(searchTwitter)
library(pracma)
library(twitteR)
library(lexiconPT)
library(syuzhet)
library(tweetbotornot)
library(plotly)
library(translateR)

tagPesquisada <- "MP de Bolsonaro"

tweets <- search_tweets(tagPesquisada, include_rts = FALSE, n = 18000, retryonratelimit = TRUE, lang = "pt")

#users <- search_users("#impeachmentRodrigoMaia",
#                     n = 500)

## Search between two dates

#api_key <- "1roTr9unhjhU1c3Yk1RRyn9sN"
#api_secret_key <- "apHe9exUDv52pGvelaAa5kzB88X7Vev9rtmBLOiM1kEfwWXb9a"
#access_token <- "2507354983-KRq6MTyKY7hyOtgAIGgq4stW4FYMkdOaNsT9fjX"
#access_token_secret <- "P6d3pokpFhmfePsxQOVhDsTEJITo2C0T38jSWymCmg3po"

## autenticar via web browser
#token <- create_token(
#  app = "AccessAPITry",
#  consumer_key = api_key,
#  consumer_secret = api_secret_key,
#  access_token = access_token,
#  access_secret = access_token_secret)

#setup_twitter_oauth(api_key, api_secret_key, access_token, access_token_secret)
#tweets <-          searchTwitter('@apyus',n = 1000, since='2020-03-06 11:00:00', until='2020-03-07 05:00:00')

## Frequência dos tweets
ts_plot(tweets, "1 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = tagPesquisada,
    caption = ""
  )

# Localização dos tweets

tweets %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  filter(location != '' & location != 'Brasil' & location != 'Brazil' & location != 'Sao Paulo/Brasil' & location != 'São Paulo' & location != 'Rio de Janeiro'
         & location != 'Anarkilópolis' & location != 'Rio de Janeiro - BRASIL'
         & location != 'Rio de Janeiro - BRASIL' & location != 'Sao Paulo, Brazil') %>%
  top_n(9) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Local",
       y = "Quantidade",
       title = tagPesquisada)

# Transformando localidade em lat e long, usando api do google maps
for (i in 1:length(tweets$location)){
  if (tweets$location[[i]] != ''){
    if (is.null(lookup_coords(tweets$location[i])$point)){
      #tweets$lat[i] = lookup_coords('china')$point[1]
      #tweets$long[i] = lookup_coords('china')$point[2]
      #china
      tweets$lat[i] = 35.86166
      tweets$long[i] = 104.19540
    } else {
      tweets$lat[i] = lookup_coords(tweets$location[i])$point[1]
      tweets$long[i] = lookup_coords(tweets$location[i])$point[2]
    }
  } else {
    #tweets$lat[i] = lookup_coords('china')$point[1]
    #tweets$long[i] = lookup_coords('china')$point[2]
    #china
    tweets$lat[i] = 35.86166
    tweets$long[i] = 104.19540
  }
}


# Mapa estático
world_basemap <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()

world_basemap

world_basemap +
  geom_point(data = tweets, aes(x = long, y = lat),
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8),
                        breaks = c(250, 500, 750, 1000)) +
  labs(title = tagPesquisada)


#Mapa com Interações
site_locations <- leaflet(tweets) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~long, lat = ~lat, popup = ~tweets$text,
                   radius = 8, stroke = FALSE)

site_locations


# usuarios participantes da tag

tweets %>%
  count(screen_name, sort = TRUE) %>%
  mutate(screen_name = reorder(screen_name,n)) %>%
  na.omit() %>%
  top_n(15) %>%
  ggplot(aes(x = screen_name,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Perfil",
       y = "Frquência de impulsionamento",
       title = tagPesquisada)


## historico de usuarios

usuariohist <- "@veramagalhaes"
historicoUsuario <- get_timelines(c(usuariohist, "@camposmello", "@pedrodoria", "@JoelPinheiro85",
                                    "@monicabergamo", "@Rconstantino"
                                    ), n = 10000)

## frequencia de tweets por usuario
historicoUsuario %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("1 days", trim = 1L) +
  #ggplot(aes(x = created_at, y=count(screen_name))) +
  #ggplot2::geom_point() +
  #ggplot2::geom_curve() +
  ggplot2::geom_line(size = 1.5) +
  #ggplot2::geom_smooth(col="blue") +
  #geom_histogram(alpha = 0.5, fill = "#5EB296", colour = "#4D4D4D") +
  #ggplot2::draw_key_timeseries() + 
  #ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = ""
  )

historicoUsuario %>% 
  ggplot(aes(x= created_at)) +
  geom_histogram(alpha = 0.5, fill = "#5EB296", colour = "#4D4D4D") +
  
  ggtitle("LOOKING AT DEATH", subtitle = "Lots of places don't have much death") +
  labs(x= "Deaths", y= "Count")

## limpeza
tweets$stripped_text <- gsub("http.*","",  tweets$text)
tweets$stripped_text <- gsub("https.*","", tweets$stripped_text)



tweets_clean <- tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

tweets_clean_date <- tweets %>%
  dplyr::select(stripped_text, created_at) %>%
  unnest_tokens(word, stripped_text)


word <- stopwords("portuguese")

word <- c(word, "maia", "impeachmentrodrigomaia", "é", "lá", "rt", "pra", "vai", "tag","somostodosbolsonaros", "de",
          "bolsonaro", "brasil", "vamos", "presidente", "governo", "tá", "jairbolsonaro", "dia", "meta", "lugar", "povo",
          "melhor", "ver", "bem", "vou", "aqui", "deixar", "quer", "lutar", "tudo", "aí", "verdade", "quanto", "gente",
          "bora", "importa", "cara", "15", "números", "sendo", "tendo", "primeiro", "tts", "apyus", "magalhães",
          tagPesquisada, "dia15brasilnasruas", "dia15euvou", "dia15porbolsonaro", "dia15pelobrasil"
          , "somostodosgeneralheleno", "ser", "veto52simaberto", "q", "vc", "somostodosbolsonaro",
          "todos", "dia15euvoupelojair", "todos", "somostodosgabrielmonteiro", "somostodosgrabrielmonteiro",
          "vcs", "dia15vaisergigante", "joaoamoedonovo", "foinoprimeiroturno",
          "turno", "_sou", "ainda", "agora", "assim", "brpolitico", "bom",
          "coisa", "cada", "100", "batoré", "3u20e38u20e3", "dar", "alvarodias",
          "caroldetoni", "corona", "coronavírus", "coronavirus", "vírus", "virus")

cleaned_tweet_words_date <- tweets_clean_date %>%
  anti_join(as.data.frame.character(word))

cleaned_tweet_words <- tweets_clean %>%
  anti_join(as.data.frame.character(word))

palavrasFreq <- cleaned_tweet_words_date %>%
  dplyr::filter(created_at > "2020-03-16") %>%
  count(word, sort = TRUE) %>%
  top_n(500) %>%
  mutate(word = reorder(word, n))

View(palavrasFreq$word)

# frequencia palavras
cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = tagPesquisada
       )

# Parear ngramas
tweets_paired_words <- cleaned_tweet_words %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

tweets_paired_words %>%
  count(paired_words, sort = TRUE)

#ngrama
tweets_paired_words %>%
  count(paired_words, sort = TRUE) %>%
  top_n(15) %>%
  mutate(paired_words = reorder(paired_words, n)) %>%
  ggplot(aes(x = paired_words, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = tagPesquisada
  )

tweets_separated_words <- tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

tweets_filtered <- tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

words_counts <- tweets_filtered %>%
  count(word1, word2, sort = TRUE)

# network
words_counts %>%
  filter(n >= 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  # geom_edge_link(aes(edge_alpha = n, edge_width = n))
  # geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag",
       subtitle = "Text mining twitter data ",
       x = "", y = "")


# Wordcloud
texto_vj <- VCorpus(VectorSource(enc2native(cleaned_tweet_words$word)))
texto_vj <- tm_map(texto_vj, content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))
texto_vj <- tm_map(texto_vj, content_transformer(tolower))
texto_vj <- tm_map(texto_vj, removePunctuation)


texto_vj <- tm_map(texto_vj,removeWords, word)

matriz_vj <- DocumentTermMatrix(texto_vj)
frequencia_vj <- colSums(as.matrix(matriz_vj))   

matriz_vj_clean <- removeSparseTerms(matriz_vj, 0.99) 
frequencia_vj <- colSums(as.matrix(matriz_vj_clean)) 

df_cloud2 <- as.data.frame(frequencia_vj)
df_cloud2[,2] = df_cloud2[,1]
df_cloud2[,1] = names(frequencia_vj)
wordcloud2(df_cloud2)

# traduzindo o lexicon para o portgues
#sentimento <- get_sentiments("bing")
#sentimento <- translate(dataset = get_sentiments("bing"), source.lang = 'en', content.field = 'word', target.lang = 'pt', google.api.key = 'AIzaSyC1GUeASCiDJemAHJjclE4RgSRD2BS-Fhg')


#Bing <- cleaned_tweet_words %>% inner_join(get_sentiments("bing"), by="word")

#sentimentosPorPalavra <- cleaned_tweet_words

#sentimentosPorPalavra$sentiment <- get_sentiment(cleaned_tweet_words$word, method='nrc', language='portuguese')
#sentimentosPorPalavra$sentiment <- ifelse(get_sentiment(cleaned_tweet_words$word, method='nrc', language='portuguese') < 0, 'negativo', 'positivo')


# usando lexiconPT - palavras pos e neg
getSentimentos <- sentiLex_lem_PT02
names(getSentimentos)[1] <- 'word'
head(getSentimentos[c('word', 'polarity')])
getSentimentosPT <- getSentimentos[c('word', 'polarity')]

uniao <- cleaned_tweet_words %>% inner_join(getSentimentosPT, by="word")
uniao$polarity <- ifelse(uniao$polarity < 0, 'negativo', 'positivo')

names(uniao)[2] <- 'sentiment'

b1 <- uniao %>% count(word, sentiment, sort=TRUE) %>%
  filter(word != 'assassino' & word != 'bandido' & word != 'criminoso' & word != 'preso' & word != 'estuprado' ) %>%
  group_by(sentiment) %>% arrange(desc(n)) %>% slice(1:15) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales="free_y") +
  labs(x="", y="frequência", title=tagPesquisada) +
  scale_fill_manual(values = c("positivo"="green", "negativo"="red"))
b1


# ts de sentimentos por dia
uniao <- cleaned_tweet_words_date %>% inner_join(getSentimentosPT, by="word")
uniao$polarity <- ifelse(uniao$polarity < 0, 'negativo', 'positivo')
names(uniao)[3] <- 'sentiment'

uniao$created_at <- as.Date(uniao$created_at)

t1 <- uniao %>% group_by(created_at) %>% count(sentiment) %>%
  spread(sentiment, n) %>% mutate(score=positivo - negativo) %>%
  ggplot(aes(x=created_at, y=score)) +
  scale_x_date(limits=c(as.Date("2020-03-08"), as.Date("2020-03-12")), date_breaks = "1 day", date_labels = "%b") +
  geom_line(stat="identity", col="blue") + geom_smooth(col="red") + labs(title="tagPesquisada")
t1

# sentimentos ingles
dados$sentimento <- as.character(dados$sentimento)
sentis <- get_nrc_sentiment(translate(words))

View(tweets)

hist(x = )


a <- translateR::translate(dataset = dados, source.lang = 'en', content.field = 'sentimento', target.lang = 'es', google.api.key = 'AIzaSyC1GUeASCiDJemAHJjclE4RgSRD2BS-Fhg')

write.csv2(cleaned_tweet_words, "palavras.csv")

uniaosentIngles <- cleaned_tweet_words %>% inner_join(get_sentiments("nrc"), by="word")

uniaosentIngles$sentiment <- ifelse(uniaosentIngles$sentiment == 'trust', 'Confiança', uniaosentIngles$sentiment)
uniaosentIngles$sentiment <- ifelse(uniaosentIngles$sentiment == 'disgust', 'Repugnância', uniaosentIngles$sentiment)
uniaosentIngles$sentiment <- ifelse(uniaosentIngles$sentiment == 'fear', 'Medo', uniaosentIngles$sentiment)
uniaosentIngles$sentiment <- ifelse(uniaosentIngles$sentiment == 'joy', 'Alegria', uniaosentIngles$sentiment)
uniaosentIngles$sentiment <- ifelse(uniaosentIngles$sentiment == 'negative', 'Negatividade', uniaosentIngles$sentiment)
uniaosentIngles$sentiment <- ifelse(uniaosentIngles$sentiment == 'positive', 'Positividade', uniaosentIngles$sentiment)
uniaosentIngles$sentiment <- ifelse(uniaosentIngles$sentiment == 'sadness', 'Tristeza', uniaosentIngles$sentiment)
uniaosentIngles$sentiment <- ifelse(uniaosentIngles$sentiment == 'surprise', 'Surpresa', uniaosentIngles$sentiment)
uniaosentIngles$sentiment <- ifelse(uniaosentIngles$sentiment == 'anger', 'Raiva', uniaosentIngles$sentiment)
uniaosentIngles$sentiment <- ifelse(uniaosentIngles$sentiment == 'anticipation', 'Antecipação', uniaosentIngles$sentiment)


for (i in 1:18) {
 sentimentoCompleto <- rbind(sentimentoCompleto, "raiva")
 
}

graficoSentimento <- uniaosentIngles %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment, y=300)) +
  labs(x="", y="", title=tagPesquisada)
graficoSentimento

# checagem de bots

tweets %>%
  count(screen_name, sort = TRUE) %>%
  mutate(screen_name = reorder(screen_name,n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = screen_name,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Perfil",
       y = "Frquência de impulsionamento",
       title = tagPesquisada)

users <- unique(tweets$screen_name)

users1 <- tweets %>%
  count(screen_name, sort = TRUE) %>%
  mutate(screen_name = reorder(screen_name,n)) %>%
  na.omit()

users1 <- users1[1:100,]

data <- botornot(users1$screen_name)
data$bot <- 1
for (i in 1:nrow(data)){
  if (data$prob_bot[i] > 0.85)
    data$bot[i] = "bot"
  else
    data$bot[i] = "humano"
}

# usuarios considerados bots
for (i in 1:nrow(data)){
  
  if (data$prob_bot[i] > 0.98){
    usuariosbots <- append(usuariosbots, c(data$screen_name[i]))
    
  } else {
    
  }
  
}

# Frequencia usando os bots
tweets %>%
  count(screen_name, sort = TRUE) %>%
  mutate(screen_name = reorder(screen_name,n)) %>%
  filter(screen_name %in% usuariosbots) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = screen_name,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Perfil",
       y = "Frquência de impulsionamento",
       title = tagPesquisada)

usuariosbots 


df<-as.data.frame(data)


ggplot(data = df) + geom_bar(mapping = aes(x = bot, y = ..prop.., group = 1), stat = "count", fill=c("purple", "steelblue")) + 
  scale_y_continuous(labels = scales::percent_format()) + labs(x = "Comportamento", y = "Percentagem", title = tagPesquisada)


## Criando um dataframe separadado a cada 10%
prob_de_ser <- as.data.frame(table(cut(data$prob_bot, 5))) 

## Inserindo o nome das colunas
prob_de_ser$Var1 <- c('0-20%', '21-40%', '41-60%', '61-80%', '81-100%')

## Criando o gráfico
plot_ly(prob_de_ser,x = ~Var1,y = ~Freq, type = 'bar',
        marker = list(color = c('rgba(0,0,255,1)', 'rgba(0,0,255,1)',
                                'rgba(0,0,255,1)', 'rgba(0,0,255,1)', 'rgba(255,0,0,1)'))) %>%
  layout(title = tagPesquisada,
         xaxis = list(title = "Probabilidade de ser um robô"),
         yaxis = list(title = "Frequência"))






# Sentimento série temporal


library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)

# Keep only 3 names
don <- babynames %>% 
  dplyr::filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
  dplyr::filter(sex=="F")
  

dados <- read.csv(file.choose())

# Plot
dados %>%
  ggplot( aes(x=infectado, y=n, group=sentimento, color=sentimento)) +
  geom_line(size=1.5) +
  geom_point(size=dados$n) +
  scale_color_viridis(discrete = TRUE) +
  #ggtitle("Sentimentos no Twitter BR x Nº infectados COVID-19") +
  theme_ipsum() +
  #ylab("Nível de Sentimento") +
  #xlab("Número de Infectados") +
  labs(title = 'Infectados: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(infectado) +
  #ease_aes('linear')

anim_save("sentTwitterCovid19.gif")




ggplot(dados, aes(x=infectado, y=n, group=sentimento, color=sentimento)) +
  geom_point(size=dados$n) +
  theme_ipsum() +
  theme(legend.text=element_text(size=15), legend.title = element_text(size = 18)) +
  guides(shape = guide_legend(override.aes = list(size = 5)),
         color = guide_legend(override.aes = list(size = 8))) +
  scale_color_viridis(discrete = TRUE) +
  # gganimate specific bits:
  labs(title = 'Sentimento no Twitter BR x Nº Infectados: {frame_time}', x = 'Nº Infectados COVID-19', y = 'Nível de Sentimento') +
  transition_time(infectado) +
  ease_aes('linear')

anim_save("sentTwitterInfCovid19.gif")




library(fmsb)

# Create data: note in High school for Jonathan:
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )
colnames(sentis) <- c("raiva" , "empolgação" , "nojo" , "medo" , "alegria", "tristeza" , "surpresa" , "tranquilidade", "negatividade", "positividade" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(20,10) , rep(0,10) , data)
sentis <- rbind(rep(45,10) , rep(0,10) , sentis)
# Check your data, it has to look like this!
# head(data)

# Custom the radarChart !
radarchart( sentis  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8, title = "MP de Bolsonaro" 
)
