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
sofiSearch = searchTwitteR("SOFI2013", n = 3000, since = "2013-10-01",
    until = "2013-10-02")

## Plot the number of tweets against time
sofiSearch.df = do.call("rbind", lapply(sofiSearch, as.data.frame))
sofiSearch.df$tweetNumber = NROW(sofiSearch.df):1
## jpeg(file = "sofi_first_day_tweets.jpg", width = 600)
with(sofiSearch.df,
     plot(created, tweetNumber, type = "l",
          ylab = "Number of tweets about SOFI 2013",
          xlab = "Time (UTC 00:00)")
     )
## graphics.off()

## Only serach tweets in English
sofiSearchEn = searchTwitteR("SOFI2013", n = 3000,  lang = "en",
    since = "2013-10-01", until = "2013-10-02")
sofiSearchEn.df = do.call("rbind", lapply(sofiSearchEn, as.data.frame))
sofiInitial.df = sofiSearchEn.df[-grep("RT", sofiSearchEn.df$text), ]

## Convert text to corpus and perform standard sanitization such as to
## lower case, remove punctuation and numbers.
myCorpus = Corpus(VectorSource(sofiInitial.df$text))
myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
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
stemmedCorpus = tm_map(stemmedCorpus, gsub, pattern="sofi",
                   replacement="sofi2013")

tdm = TermDocumentMatrix(stemmedCorpus,
    control=list(wordLengths=c(1,Inf)))



## Load Sentimental Lexicon from Hu and Liu
poswords = scan(file = "positive-words.txt", what = "character",
    comment.char = ";")
negwords = scan(file = "negative-words.txt", what = "character",
    comment.char = ";")

## Plot the word cloud
wordcloudMatrix = as.matrix(tdm)
wordFreq = sort(rowSums(wordcloudMatrix), decreasing=TRUE)
wordFreq.df = data.frame(word = names(wordFreq), freq = wordFreq)
wordFreq.df$col = ifelse(wordFreq.df$word %in% poswords, "blue",
    ifelse(wordFreq.df$word %in% negwords, "red", "grey50"))

## jpeg("word_cloud.jpg")
with(wordFreq.df, wordcloud(words = word, freq = freq, min.freq = 0,
                            random.order = FALSE, colors = col,
                            random.color = FALSE, ordered.colors = TRUE))
## graphics.off()

## Read the score_sentiment function from Jeffrey Breen
source("score_sentiment.R")


scoring = tm_map(x = stemmedCorpus, FUN = score_sentiment,
    pos.words = poswords, neg.words = negwords)

sofiTweetScores = sapply(scoring, FUN = function(x) x$score)
plot(table(sofiTweetScores))
