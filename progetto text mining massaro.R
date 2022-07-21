install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("ggplot2")
install.packages("sentimentr")
install.packages("tidyverse")

library(sentimentr)
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)


stelle <- table(Amazon_Review_Complete_Data_669552874$Rating)
data <- data.frame(
  stars=c("1","2","3","4","5") ,  
  value=c(stelle)
)
ggplot(data, aes(x=stars, y=value)) + 
  geom_bar(stat = "identity", width=0.5)





recensioni <- Amazon_Review_Complete_Data_669552874$`Review Content`
playsolofrasi <- get_sentences(Amazon_Review_Complete_Data_669552874$`Review Content`)
playcorpus <- VCorpus(VectorSource(playsolofrasi))


# Convert the text to lower case
playpulito <- tm_map(playcorpus, content_transformer(tolower))
# Remove numbers
playpulito <- tm_map(playpulito, removeNumbers)
# Remove english common stopwords
playpulito <- tm_map(playpulito, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
playpulito <- tm_map(playpulito, removeWords, c("s", "company", "team")) 
# Remove punctuations
playpulito <- tm_map(playpulito, removePunctuation)
# Eliminate extra white spaces
playpulito <- tm_map(playpulito, stripWhitespace)
# Text stemming - which reduces words to their root form
playpulito <- tm_map(playpulito, stemDocument)

# Build a term-document matrix
playpulito_dtm <- TermDocumentMatrix(playpulito)
dtm_m <- as.matrix(playpulito_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)

# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, names.arg = dtm_d[1:5,]$word,
        col ="gray", main ="Top 5 most frequent words",
        ylab = "Word frequencies")


#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

# Find associations 
findAssocs(playpulito_dtm, terms = c("consol","game","one","control","great"), corlimit = 0.25)	

sentences<- sentimentr::get_sentences(Amazon_Review_Complete_Data_669552874$`Review Content`)
calcolo_sentiment<- sentiment(sentences)

ggplot(data = calcolo_sentiment, aes(x=calcolo_sentiment$sentiment))+
  geom_density(fill='#69b3a2',color='#e9ecef',alpha=0.8)+
  labs(title = 'SENTIMENT PER OGNI FRASE',subtitle = 'Distribuzione empirica',x='Sentiment',y='Densità')+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

# calcolo del sentiment per ogni recensione
groups<- sentiment_by(sentences)
ggplot(data=groups, aes(x=groups$ave_sentiment))+geom_density(fill='#69b3a2',color='#e9ecef',alpha=0.8)+
  labs(title = 'SENTIMENT PER OGNI RECENSIONE',subtitle = 'Distribuzione empirica',x='Sentiment',y='Densità')+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
plot(groups)



install.packages("textrank")
library("textrank")
install.packages("udpipe")
library(udpipe)
dl <- udpipe_download_model(language = "english")
str(Amazon_Review_Complete_Data_669552874$`Review Content`)
udmodel <- udpipe_load_model(file = dl$file_model)
txt <- Amazon_Review_Complete_Data_669552874$`Review Content`
x <- udpipe_annotate(udmodel,x=txt)
x <- as.data.frame(x)
str(x)
x$upos

install.packages("ctv")
library(ctv)
pkgs <- available.views()
names(pkgs) 

install.packages("data.table")
install.packages("udpipe")
install.packages("stopwords")
library(data.table)
library(stopwords)
library(udpipe)
 
biterms <- as.data.table(x)
biterms <- biterms[,cooccurrence(x=lemma,relevant=upos %in% c("NOUN", "ADJ", "VERB") & nchar(lemma)>2 & !lemma %in% stopwords("en"), skipgram = 3),by = list(doc_id)]
install.packages("BTM")
library(BTM)
set.seed(123456)
traindata <- subset(x, upos %in% c("NOUN", "ADJ", "VERB") & !lemma %in% stopwords("en") & nchar(lemma) >2)
traindata <- traindata[, c("doc_id", "lemma")]
model <- BTM(traindata,biterms = biterms, k=6, iter=2000, background=TRUE, trace=100)
install.packages("textplot")
library(textplot)
install.packages("concaveman")
library(concaveman)
install.packages("ggraph")
library(ggraph)

plot(model, top_n = 10,
     title = "BTM model", subtitle = "ps5",
     labels = c("quality", "console", "storage/memory", "controller","order/shipment","characteristics"))




#sommario sceglie le frasi più centrali
install.packages("textrank")
library("textrank")

cat(unique(x$sentence),sep="\n")
names(x)
head(x[,c("sentence_id","lemma","upos")],10)
keyw <- textrank_keywords(x$lemma,relevant=x$upos%in%c("NOUN","VERB","ADJ"))
subset(keyw$keywords,ngram>1)
install.packages("udpipe")
library(udpipe)
x$textrank_id <- unique_identifier(x,c("doc_id","paragraph_id","sentence_id"))
sentences <- unique(x[,c("textrank_id","sentence")])
head(sentences,10)
terminology <- subset(x,upos%in%c("NOUN","ADJ"))
terminology <- terminology[,c("textrank_id","lemma")]
terminology
tr <- textrank_sentences(data=sentences,terminology=terminology)#funzione chiave ma prima bisogna crerare un dataset possibie da analizzare
tr$sentences
tr$sentences_dist
tr$pagerank
vv <- sort(tr$pagerank$vector,decreasing = TRUE)
plot(vv,type = "b",ylab = "Pagerank",main="Textrank")#ci serve per capire se la distribuzione
#dell'intensita e piu o meno uniforme come cade ci interessa se abbiamo tanti dati ma l intensita cade velocemente 
#possimao considerare solo oggetti con intensita maggiore tipo ora abbiamo un calo ben costante  
s <- summary(tr,n=10)
s <- summary(tr,n=5,keep.sentence.order = TRUE)
cat(s,sep="\n")
