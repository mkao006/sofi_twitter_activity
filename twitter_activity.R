########################################################################
## Title: Monitoring SOFI 2013 release twitter activity
## Date: 2013-10-01
########################################################################

## Load libraries
library(twitteR)
library(tm)
library(wordcloud)
library(Snowball)
library(SnowballC)

## Optinal, just to read my own consumerKey and consumerSecret
myKeys = fromJSON(file = "my_twitter_keys.json")

## Initiat parameters 
reqURL = "https://api.twitter.com/oauth/request_token"
accessURL = "http://api.twitter.com/oauth/access_token"
authURL = "http://api.twitter.com/oauth/authorize"
consumerKey = myKeys$consumerKey
consumerSecret = myKeys$consumerSecret
twitCred = OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)

## Authorization with Twitter
twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem",
                     package = "RCurl"))
registerTwitterOAuth(twitCred)

## Search for total number of tweets about SOFI 2013 since official
## launch date.
sofiSearch = searchTwitteR("#SOFI2013", n = 1000, since = "2013-10-01")

## Plot the number of tweets against time
sofiSearch.df = do.call("rbind", lapply(sofiSearch, as.data.frame))
sofiSearch.df$tweetNumber = NROW(sofiSearch.df):1
with(sofiSearch.df,
     plot(created, tweetNumber, type = "l",
          ylab = "Number of tweets about SOFI 2013",
          xlab = "Time (UTC)")
     )

## Only serach tweets in English
sofiSearchEn = searchTwitteR("#SOFI2013", n = 1000,  lang = "en",
    since = "2013-10-01")
sofiSearchEn.df = do.call("rbind", lapply(sofiSearchEn, as.data.frame))


## Convert text to corpus and perform standard sanitization such as to
## lower case, remove punctuation and numbers.
myCorpus = Corpus(VectorSource(sofiSearchEn.df$text))
myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
removeURL = function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus = tm_map(myCorpus, removeURL)
myStopwords = stopwords("SMART")
finalCorpus = tm_map(myCorpus, removeWords, myStopwords)

## Stemming words
dictCorpus <- tm_map(finalCorpus, stemDocument)
stemmedCorpus = tm_map(finalCorpus, stemCompletion,
    dictionary = dictCorpus)

## Hack to replace words which are not stemmed properly
stemmedCorpus = tm_map(stemmedCorpus, gsub, pattern="hungry",
                   replacement="hunger")
stemmedCorpus = tm_map(stemmedCorpus, gsub, pattern="chronically",
                   replacement="chronic")

tdm = TermDocumentMatrix(stemmedCorpus,
    control=list(wordLengths=c(1,Inf)))

## Plot the word cloud
wordcloudMatrix = as.matrix(tdm)
wordFreq = sort(rowSums(wordcloudMatrix), decreasing=TRUE)
grayLevels = gray((wordFreq+10) / (max(wordFreq)+10))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3,
          random.order=F, colors=grayLevels)


## Load Sentimental Lexicon from Hu and Liu
poswords = scan(file = "positive-words.txt", what = "character",
    comment.char = ";")
negwords = scan(file = "negative-words.txt", what = "character",
    comment.char = ";")

## Read the score_sentiment function from Jeffrey Breen
source("score_sentiment.R")


scoring = tm_map(x = stemmedCorpus, FUN = score_sentiment,
    pos.words = poswords, neg.words = negwords)

sofiTweetScores = sapply(scoring, FUN = function(x) x$score)
plot(table(sofiTweetScores))


## Plot network association (Incomplete)
networkMatrix = as.matrix(tdm)
networkMatrix[networkMatrix >= 1] = 1
networkMatrix = networkMatrix %*% t(networkMatrix)


library(igraph)
# build a graph from the above matrix
g <- graph.adjacency(networkMatrix, weighted=TRUE, mode="undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)


## library(network)
## test = network(networkMatrix)
## pdf(file = "networkTest.pdf", width = 20, height = 20)
## plot(test, displaylabels = TRUE, label.col = "steelblue",
##      label.cex = 2, vertex.border = "white",
##      vertex.col = "skyblue", edge.col = rgb(0, 0, 0, alpha = 0.5))
## graphics.off()
## system("evince networkTest.pdf&")
## system("rm networkTest.pdf")


