install.packages("twitteR")
install.packages("ROAuth")
install.packages("stringr")
install.packages("httr")
install.packages("wordcloud")
install.packages("sentimentr")
install.packages("syuzhet",dependencies = T)
install.packages("tm")
install.packages("base64enc")
install.packages("openssl")
install.packages("httpuv")
install.packages("ggplot2")
install.packages("rlang")
install.packages("pkgconfig")
install.packages("swirl")
library(httpuv)
library(rlang)
library(plotly)
library(openssl)
library(base64enc)
library(twitteR)
library(ROAuth)
library(stringr)
library(httr)
library(wordcloud)
library(sentimentr)
library(syuzhet)
library(plyr)
library(tm)
library(ggplot2)


consumer_key = "tOgqNoSMCiyq5wvpKcHE8baCL"
consumer_secret = "JKIEN5ub0Cs6lapDgw8s3ocMON9JHD5PJWdg70DJPg9DzU2hwQ"
access_token = "10902171841090217184-f0pvV4ZKxYXlm7DQSkGlGFIFvmWROMdhjo9ZpFr"
access_secret = "UldbkS4REgE7865oGdy4JhUI46iia1bhfBSTFS9lQLsDp"




setup_twitter_oauth(consumer_key,consumer_secret)#,access_token,access_secret,)

sometweets=searchTwitter(searchString = "software defined network",n=1000,since = "2016-01-01",lang = "en")

View(sometweets)
class(sometweets)

length.sometweets=length(sometweets)

some_tweets.df=ldply(.data = sometweets,.fun = function(t) t$toDataFrame())

write.csv(some_tweets.df,"tweets.csv")

sometext=sapply(sometweets, function(x) x$getText())
class(sometext)

    
sometext = gsub("-", " ", sometext)
sometext =  gsub("&", " ", sometext)
sometext =  gsub("[[:punct:]]", " ", sometext)
sometext =  gsub("[[:digit:]]", "", sometext)
sometext =  gsub("http\\w+", "", sometext)
sometext =  gsub("\n", " ", sometext)
sometext =  gsub("[ \t]{2,}", "", sometext)
sometext =  gsub("^\\s+|\\s+$", "", sometext)
sometext =  tolower(sometext)
sometext=gsub("rt"," ",sometext)
#gsub()

write.csv(sometext,"cleanedtweets.csv")
sometext1=Corpus(VectorSource(sometext))
sometext1=tm_map(sometext1,removePunctuation)
sometext1=tm_map(sometext1,content_transformer(tolower))
sometext1=tm_map(sometext1,removeWords,stopwords(kind = "en"))
sometext1=tm_map(sometext1,stripWhitespace)

colopalet=brewer.pal(08,"Dark2")


wordcloud(words = sometext1,min.freq = 5,max.words = Inf, 
          width=1000,height=1000,random.order = F,colors = colopalet)


install.packages("pkgconfig",
                 repos = c("http://rstudio.org/_packages",
                           "http://cran.rstudio.com"))
mysentiments=syuzhet::get_nrc_sentiment(sometext)
syuzhet::get_percentage_values(sometext)
sentimentscores=data.frame(colSums(mysentiments[,]))
names(sentimentscores)="scores"
sentimentscores

sentimentscores=cbind("sentiment"=rownames(sentimentscores),sentimentscores)

#windows()
#ggplot2::ggplot(sentimentscores,aes(sentiment,scores))+geom_bar(stat = "identity",fill="blue")+xlab("sentiment")+ylab("scores")+ggtitle("total sentiment based on tweets")

p <- plot_ly(sentimentscores, labels = ~sentiment, values = ~scores, type = 'pie') %>%
  layout(title = 'Sentiments around software defined network(SDN)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

Sys.setenv("plotly_username"="gangadhardixit")
Sys.setenv("plotly_api_key"="oqpv4UmNtSK6X0CeGTQb")
plotly::plotly_POST(p,filename = "SDN")


# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="pie/donut")
chart_link

