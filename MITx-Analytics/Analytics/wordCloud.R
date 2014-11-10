tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
library(tm)


corpusTweet = Corpus(VectorSource(tweets$Tweet))
corpusTweet = tm_map(corpusTweet, tolower)
corpusTweet = tm_map(corpusTweet, removePunctuation)
corpusTweet = tm_map(corpusTweet, removeWords, stopwords("english"))
dtmTweets = DocumentTermMatrix(corpusTweet)

allTweets = as.data.frame(as.matrix(dtmTweets))
summary(allTweets)
str(allTweets)
length(allTweets)

library(wordcloud)



allTweets$apple = NULL


library(RColorBrewer)

# wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2,.25),colors=brewer.pal(9, "Blues"))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2,.25),colors=brewer.pal(9, "Blues")[c(5,6,7,8,9)])


