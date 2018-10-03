'
NAME: Anishi Mehta
DATE: 17th December

Problem Statement: Extracting and analyzing tweets associated with #gayrights.

Observations:       Initially, the code works on cleaning up the text obtained. It removes extra characters, white spaces and converts to lower case.
                    Next, a word cloud is generated. 
                    The terms with higher frequency were LGBTQ, GAY, EQUALITY, LOVE, HUMANRIGHTS. MARRIAGE was also a high frequency word owing to the marital laws being debated.
                    This showed that the tweets about #gayrights also talk about these issues.
                    Furthermore, I found certain associations between the words. FREEDOM was associated with DEMOCRACY, etc.
                    For the sentiment analysis, I found that most of the tweets were positive i.e. in support of #gayrights.
                    This data was also clustered by sentiments to know the varying degrees of emotions, as simply positive or negative is not enough.
                    This showed that twitter users tend to be in active support of #gayrights and the other LGBTQ community.
                    Lastly, the tweets were categorized based on the words that were used in the tweets.
                    Since there would be limited number of topics, I decided to divide the tweets into three clusters.
                    The outputs were then plotted based on the probabilities of a tweet being assigned a topic.
                    The first topic had words like history, archives, support, equality and #wipehomophobia and hence the tweets under this topic leaned towards the situation in the past and the need to change it. 
                    These tended to be towards the negative side. 
                    The second had words like #lovewins, marriage, same, pride, etc. showing that those tweets were towards the recent marital laws passed.
                    @jiggyhuncks had a lot of tweets under this topic.
                    The third topic had keywords like lgbt, rights, humanrightsday, etc. and tended to be towards the positive side and talked not only about gays, but rights for the whole LGBTQ community.
                    The current trending tweets of Bombay were looked at. These reflected the current mood of the country and the topics currently in question.
                    They were #DeMonetisation, #INDvENG, Modi, etc.

NOTE: The functions and the data are not very reliable. The cleaning of the corpus leads to certain anomalies most times.
      This led to anomalies in the results obtained, as well as the analysis.
      Moreover, the wordcloud and TermDocumentMatrix function give errors at times and work at other times without any changes made to the code.
      Lastly, the number of tweets extracted varied between 250 to 400 depending on the connection.
'

'
install.packages("twitteR")
install.packages("ROAuth")
install.packages("wordcloud")
install.packages("sentimentr")
install.packages("tm")
install.packages("SnowballC")
install.packages("plyr")
install.packages("stringr")
install.packages("topicmodels")
install.packages("ggplot2")
'

library(twitteR)
library(ROAuth)
library(wordcloud)
library(tm)
library(sentimentr)
library(SnowballC)
library(plyr)
library(stringr)
library(topicmodels)

api_key <- "xxxxxxxxxxx"
api_secret <- "xxxxxxxxxxx"
access_token <- "xxxxxxxxxxx"
access_secret <- "xxxxxxxxxxx"

setup_twitter_oauth(api_key, api_secret, access_token, access_secret)
tweetsCDSA <- searchTwitter("#gayrights", n = 500)

#convert tweets (unstructured data) to data frame
tweets <- twitteR::twListToDF(tweetsCDSA)
View(tweets)
#Remove emojis and convert to ASCII
tweet_text <- sapply(tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
corpus <- Corpus(VectorSource(tweet_text))

#Cleaning up the data
removeURL <- function(x) gsub("http[[:alnum:]]*", " ", x)
corpus <- tm_map(corpus, removeURL)
corpus <- tm_map(corpus, removePunctuation, lazy = TRUE)
corpus <- tm_map(corpus, removeWords, stopwords("english"), lazy = TRUE)
corpus <- tm_map(corpus, removeWords, c("the", "via", "can", "know", "you", "your", "this", "them", "for"), lazy = TRUE)
corpus <- tm_map(corpus, stripWhitespace, lazy = TRUE)
corpus <- tm_map(corpus, stemDocument, lazy = TRUE)
corpus <- tm_map(corpus, removeNumbers, lazy = TRUE)
corpus <- tm_map(corpus, content_transformer(tolower), lazy = TRUE)
cleanset <- tm_map(corpus, PlainTextDocument, lazy = TRUE)

#Generating Word Cloud for the data
pal <- brewer.pal(8, "Dark2")
set.seed(123)
wordcloud(words = cleanset, min.freq = 1, max.words = 100, scale = c(3, 0.9), random.order = FALSE, rot.per = 0.35, use.r.layout = FALSE, colors = pal, lazy = TRUE)

#Generating the Term Document Matrix
tdm <- TermDocumentMatrix(cleanset)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word = names(v), freq=v)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, main ="Most frequent words", ylab = "Word frequencies")

#Naming columns of tdm by Tweet serial number. This is of use during Topic Modeling.
tweet_no <- "1"
for (i in 2:ncol(tdm)){
  tweet_no <- c(tweet_no, i)
}
colnames(tdm) <- tweet_no
#Taking transpose of tdm
dtm <- t(tdm)

#Finding Correlations between terms
findAssocs(dtm, terms = "gay", corlimit = 0.3)
findAssocs(dtm, terms = "rights", corlimit = 0.4)
findAssocs(dtm, terms = "lgbtq", corlimit = 0.3)
findAssocs(dtm, terms = "religion", corlimit = 0.3)
findAssocs(dtm, terms = "equality", corlimit = 0.3)

#Sentiment Analysis of the tweets. Divided into 4 clusters.
tweet_senti <- sentiment(tweet_text, sentiword)
kmeans_tweet <- kmeans(tweet_senti[,4], 4)
plot(sentiment~element_id, data = tweet_senti, col = kmeans_tweet$cluster, main = "Sentiments of Tweets")

#Topic Modeling. Dividing into k topics.
SEED = sample(1:1000000, 1)
k = 5
ldaOut <- LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100,    iter = 1000))
ldaOut.topics <- as.matrix(topics(ldaOut))        #Topic for each tweet
ldaOut.terms <- as.matrix(terms(ldaOut, 10))      #Six high weightage terms for each topic
topicProbab <- as.data.frame(ldaOut@gamma)        #Probabilities of each topic for each tweet
plot(V1~V2, data = topicProbab, col = ldaOut.topics)

#Analyzing trends of Bombay
trend <- availableTrendLocations()
trend
location <- getTrends(2295411)
head(location)


