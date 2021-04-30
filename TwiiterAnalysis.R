

library(twitteR)
library(reshape)
library(wordcloud)
library(igraph)
library("base64enc")
library("ROAuth")
library("devtools")
library("memoise")
library("whisker")
library("rstudioapi")
library("git2r")
library("withr")
library("rjson")
library("bit64")
library ("httr")
library ("httpuv")
library(qdapRegex)
library(RTextTools)
library(tm)
library(tidyr)
library(dplyr)
library(SentimentAnalysis)
data(DictionarHE)
#
#
#
consumerKey <- "VPUGAVX7KmgQWTjCFHskef7p0"
consumerSecret <- "JuveJmxzPrAXhmjmlRDBddpOFgRkq3FKEP1SnnWoGhQqgGJwVq"
accessToken <- "794784653919993856-9svGGq2M3t2VuIHSHlchTcAJ7S1tSlH"
accessTokenSecret <- "80Zw8tPmG0X1pDRRkn1CpjdwDg5P2pcpGFeeyuVuT6Ihk"
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
origop <- options("httr_oauth_cache")
options(httr_oauth_cache = TRUE)
#
#
#
tweets=searchTwitter("#GOT", n=1000,lang = "en")
df = do.call("rbind", lapply(tweets, as.data.frame))
#
write.csv(df, "D://Desktop/DirectMarketing/GOTTweets.csv", row.names=FALSE)
#

GOTSearch = read.csv("D://Desktop/DirectMarketing/GOTTweets.csv",header = TRUE,stringsAsFactors = F,sep=",")
str(GOTSearch)
head(GOTSearch,5)
GOTSearchText  = as.vector(GOTSearch$text)


cleanTweet = gsub("rt|RT", "", GOTSearchText) # remove Retweet
cleanTweet = gsub("http\\w+", "", cleanTweet)  # remove links http
cleanTweet = gsub("<.*?>", "", cleanTweet) # remove html tags
cleanTweet = gsub("@\\w+", "", cleanTweet) # remove at(@)
cleanTweet = gsub("[[:punct:]]", "", cleanTweet) # remove punctuation
cleanTweet  = gsub("\r?\n|\r", " ", cleanTweet) # remove /n
cleanTweet = gsub("[[:digit:]]", "", cleanTweet) # remove numbers/Digits
cleanTweet = gsub("???|???|???|???|???|???|???|???|???|???", "", cleanTweet) #  asian letters
cleanTweet = gsub("[ |\t]{2,}", "", cleanTweet) # remove tabs
cleanTweet = gsub("^ ", "", cleanTweet)  # remove blank spaces at the beginning
cleanTweet = gsub(" $", "", cleanTweet) # remove blank spaces at the end 



library(syuzhet)
GOTSentiment = get_nrc_sentiment(cleanTweet)
head(GOTSentiment,5)


GOTFinalData = cbind(GOTSearch,GOTSentiment)

plotData1 =gather(GOTFinalData,"sentiment","values",17:24)  %>% 
  group_by( sentiment) %>%
  summarise(Total = sum(values))
library(ggplot2)
ggplot(data = plotData1, aes(x = plotData1$sentiment, y = plotData1$Total)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Emotions") + ylab("Total") + ggtitle("Emotion for Search Term Trump")+
  geom_text(aes(label =   plotData1$Total), position = position_dodge(width=0.75), vjust = -0.25)
plotData2 =gather(GOTFinalData,"Polarity","values",25:26)  %>% 
  group_by( Polarity) %>%
  summarise(Total = sum(values))

ggplot(data = plotData2, aes(x = plotData2$Polarity, y = plotData2$Total)) +
  geom_bar(aes(fill = plotData2$Polarity), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total") + ggtitle("Sentiment for Search Term Trump")+
  geom_text(aes(label =   plotData2$Total), position = position_dodge(width=0.75), vjust = -0.25)



library(stringr)
library(ggplot2)
vector = GOTSearch$text
Corpus <- Corpus(VectorSource(vector))
Corpus = tm_map(Corpus,removeNumbers)
Corpus = tm_map(Corpus,str_replace_all,pattern = "http\\w+", replacement =" ")
Corpus = tm_map(Corpus,str_replace_all,pattern = "<.*?>", replacement =" ")
Corpus = tm_map(Corpus,str_replace_all,pattern = "@\\w+", replacement =" ")
Corpus = tm_map(Corpus,str_replace_all,pattern ="\\=", replacement =" ")
Corpus = tm_map(Corpus,str_replace_all,pattern = "[[:punct:]]", replacement =" ")
Corpus = tm_map(Corpus,str_replace_all,pattern = "amp", replacement =" ")
Corpus = tm_map(Corpus,removeWords, words= stopwords("en"))
Corpus = tm_map(Corpus,tolower)
Corpus=tm_map(Corpus,function(x) removeWords(x,stopwords()))
tdm = TermDocumentMatrix(Corpus)
tdm
idx <- which(dimnames(tdm)$Terms %in% c("got", "fight", "winter"))
as.matrix(tdm[idx, 21:30])
idx
(freq.terms <- findFreqTerms(tdm, lowfreq = 20))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 20)
df <- data.frame(term = names(term.freq), freq = term.freq)
# library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))
m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)

par(bg = "white")
wordcloud(Corpus, colors = c("red",  "green", "yellow"), scale = c(6, 0.5), random.color = TRUE, rot.per = 0.5,   min.freq = 5, font = 2, max.word=200,family = "serif")
wordcloud(words = Corpus, max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
wordcloud(words = names(word.freq), max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]
# plot word cloud
# library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)







##################################Term Network
inspect(tdm[202:205, 1:5])
inspect(tdm[c("got", "gamesofthrones", "blood"),1:5] )
inspect(tdm[c("got", "war")])

# Network analysis
# create matrix
ukraine.m <- as.matrix(tdm[c("got", "show","ever","greatest","want","fans","producing"),1:200] )
# to boolean matrix
ukraine.m[ukraine.m>=1] <- 1
# to term adjacency matrix
# %*% is product of 2 matrices
ukraine.m2 <- ukraine.m %*% t(ukraine.m)
# build graph with igraph ####
library(igraph)
# build adjacency graph
ukraine.g <- graph.adjacency(ukraine.m2, weighted=TRUE, mode="undirected")
# remove loops
ukraine.g <- simplify(ukraine.g)
# set labels and degrees of vertices
V(ukraine.g)$label <- V(ukraine.g)$name
V(ukraine.g)$degree <- degree(ukraine.g)
# plot layout fruchterman.reingold
layout1 <- layout.fruchterman.reingold(ukraine.g)
plot(ukraine.g, layout=layout1, vertex.size=20, 
     vertex.label.color="darkred")
# change label size of vertices (nodes) V()
# change the edge color (connections) E()
V(ukraine.g)$label.cex <- 2.2 * V(ukraine.g)$degree / max(V(ukraine.g)$degree) + .2
V(ukraine.g)$label.color <- rgb(0, 0, 0.2, 0.8)
V(ukraine.g)$frame.color <- NA
egam <- (log(E(ukraine.g)$weight) + .4) / max(log(E(ukraine.g)$weight) + .4)
E(ukraine.g)$color <- rgb(.5, .5, 0, egam)
E(ukraine.g)$width <- egam
# improved plot
plot(ukraine.g, layout1)






############################RETWEET NETWORK
alltweets<-GOTSearch[1:20,]
alltweets<-GOTSearch
#create an edge-list for retweet network
sp = split(alltweets, alltweets$isRetweet)
rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))
el = as.data.frame(cbind(sender = tolower(rt$sender), receiver = tolower(rt$screenName)))
# el = count(el, sender, receiver) 
el[1:5,] #show the first 5 edges in the edgelist
rt_graph <- graph_from_data_frame(d=el, directed=T)
glay = layout.fruchterman.reingold(rt_graph) 
plot(rt_graph)
# install.packages("networkD3")
library(networkD3)
wc <- cluster_walktrap(rt_graph)
members <- membership(wc)
d3_rt <- igraph_to_networkD3(rt_graph, group = members)
forceNetwork(Links = d3_rt$links, Nodes = d3_rt$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group')
#########################################""




 #################find statistics 
g <-rt_graph
ecount(g)
vcount(g)
E(rt_graph)[1:50] 
V(rt_graph)[1:50]
#Calculate density:The proportion of present edges from all possible edges in the network.
edge_density(rt_graph, loops=F) #for an undirected network
#Calculate reciprocity:The proportion of reciprocated ties (for a directed network).
reciprocity(rt_graph)
centr_degree(rt_graph, mode = c("in"), loops = TRUE,normalized = TRUE)$centralization
#Calculate transitivity:the probability that the neighbors of a vertex are connected. 
transitivity(rt_graph, type="local")
#Calculate the length of the longest path between two vertices in the network
diameter(rt_graph, directed=F, weights=NA) 
#Calculate in-degree centrality
indegree <- sort(degree(rt_graph,mode = "in"),decreasing = TRUE)
indegree[1:20] #show the top vertices by in-degree 
#Calculate betweenness centrality
bt <- sort(betweenness(rt_graph, directed=F, weights=NA), decreasing = TRUE)
bt[1:20] #show the top vertices by betweenness centrality 
#Calculate closeness centrality: measures how many steps is required to access every other vertex from a given vertex
cc <- sort(closeness(rt_graph, mode="all", weights=NA), decreasing = TRUE)
cc[1:20] #show the top vertices by closeness centrality 
ceb <- cluster_edge_betweenness(rt_graph) #Community detection based on edge betweenness (Newman-Girvan)
length(ceb)










##########################Classification
# library(naivebayes)
library(e1071)
# build dtm 
matrix= create_matrix(GOTFinalData[,1], language="english",    removeStopwords=FALSE, removeNumbers=TRUE,                        stemWords=FALSE) 
matrix
mat = as.matrix(matrix) 
mat
#if "positive" is equal to 2 or 3, it will be 1(means positive),otherwise it will be 0(means negative).
GOTFinalData[,3]<-ifelse(GOTFinalData[,3]>0,1,0)
#naiveBayes
classifier = naiveBayes(mat[1:200,], as.factor(GOTFinalData[1:200,3]) )
# classifier
# test the validity 
predicted = predict(classifier, GOTFinalData[201:250,3]); 
predicted
table(GOTFinalData[201:250, 1], predicted)
recall_accuracy(GOTFinalData[201:250, 3], predicted)

# build the data to specify response variable, training set, testing set. 
container = create_container(matrix, as.numeric(as.factor(GOTFinalData[,3])),  trainSize=1:200, testSize=201:250,virgin=FALSE)     
GOTFinalData[,3]
models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))

results = classify_models(container, models)
results
# accuracy table 
table(as.numeric(as.factor(GOTFinalData[201:250, 3])), results[,"FORESTS_LABEL"]) 
table(as.numeric(as.factor(GOTFinalData[201:250, 3])), results[,"MAXENTROPY_LABEL"])
# recall accuracy 
recall_accuracy(as.numeric(as.factor(GOTFinalData[201:250, 3])), results[,"FORESTS_LABEL"]) 
recall_accuracy(as.numeric(as.factor(GOTFinalData[201:250, 3])), results[,"MAXENTROPY_LABEL"]) 
recall_accuracy(as.numeric(as.factor(GOTFinalData[201:250, 3])), results[,"TREE_LABEL"]) 
recall_accuracy(as.numeric(as.factor(GOTFinalData[201:250, 3])), results[,"BAGGING_LABEL"])
recall_accuracy(as.numeric(as.factor(GOTFinalData[201:250, 3])), results[,"SVM_LABEL"])

# model summary 
analytics = create_analytics(container, results) 
summary(analytics) 
head(analytics@document_summary)
analytics@ensemble_summary


