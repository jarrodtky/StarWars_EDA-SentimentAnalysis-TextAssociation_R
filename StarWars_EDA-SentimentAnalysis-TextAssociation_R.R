# Load libraries
library(tidyverse)
library(tm)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(reshape2)
library(radarchart)
library(RWeka)
library(plotly)
library(highcharter)
library(viridis)
library(plotrix)
library(ggthemes)
library(ggplot2)
library(qdap)
library(igraph)
library(ggraph)
library(visNetwork)
library(textdata)
library(RColorBrewer)
library(showtext)

# Check working directory
#getwd()
# Set own working directory
#setwd("")



# Analysis A: Text Mining Analysis

# Read/load the data
ep4 <- read.table("SW_EpisodeIV.txt")
ep5 <- read.table("SW_EpisodeV.txt")
ep6 <- read.table("SW_EpisodeVI.txt")

trilogy <- bind_rows(ep4, ep5, ep6)   # bind_rows will display missing rows as NA (selected)
#trilogy <- rbind(ep4, ep5, ep6)      # rbind shows error if rows not the same 
#rm(ep4, ep5, ep6)                    # rm() is basically 'remove{base}', it is used to Remove Objects from a Specified Environment.



# Functions used throughout the R code. 
# Text transformations
cleanCorpus <- function(corpus){
  
  corpus <- tm_map(corpus, removePunctuation) #remove Punctuation
  corpus <- tm_map(corpus, stripWhitespace) #remove whitesapces
  corpus <- tm_map(corpus, content_transformer(tolower)) #transform text to lower case
  v_stopwords <- c(stopwords("english"), c("thats","weve","hes","theres","ive","im",
                                           "will","can","cant","dont","youve","us",
                                           "youre","youll","theyre","whats","didnt"))
  corpus <- tm_map(corpus, removeWords, v_stopwords) #remove stop words
  corpus <- tm_map(corpus, removeNumbers) #remove numbers
  return(corpus)
  
}

# Most frequent terms 
frequentTerms <- function(text){
  
  df_corpus <- VCorpus(VectorSource(text)) #creating corpus from text column
  df_corpus_clean <- cleanCorpus(df_corpus) #cleaning the corpus using the cleancorpus function
  df_corpus_tdm <- TermDocumentMatrix(df_corpus_clean) #Creating a term document matrix
  df_corpus_tdm <- removeSparseTerms(df_corpus_tdm, sparse = 0.999) #removing sparse terms from TDM
  df_corpus_m <- as.matrix(df_corpus_tdm) #Converting the TDM to a Matrix
  word_freq <- sort(rowSums(df_corpus_m), decreasing=TRUE) #convering matrix to data frame with terms and term frequency
  df <- data.frame(word=names(word_freq), freq=word_freq)
  return(df)
  
}


# Define bigram tokenizer 
tokenizer  <- function(x){
  
  NGramTokenizer(x, Weka_control(min=2, max=2)) #tokenizing n-grams with paired words.
  
}


# Most frequent bigrams 
# Using the same code from frequentTerms and control using the tokenizer function to create bigrams
frequentBigrams <- function(text){
  
  df_corpus_bi <- VCorpus(VectorSource(text))
  df_corpus_bi_clean <- cleanCorpus(df_corpus_bi)
  df_corpus_bi_tdm <- TermDocumentMatrix(df_corpus_bi_clean, control=list(tokenize=tokenizer))
  df_corpus_bi_tdm <- removeSparseTerms(df_corpus_bi_tdm, 0.999)
  df_corpus_bi_m <- as.matrix(df_corpus_bi_tdm)
  word_freq_bi <- sort(rowSums(df_corpus_bi_m), decreasing=TRUE)
  df_bi <- data.frame(word=names(word_freq_bi), freq=word_freq_bi)
  return(df_bi)
  
}

# clean by each character
clean_top_char <- function(dataset){
  all_dialogue <- list()
  namelist <- list()
  
  for (i in 1:9){
    
    name <- top_chars$character[i]
    dialogue <- paste(dataset$dialogue[dataset$character == name], collapse = " ")
    all_dialogue <- c(all_dialogue, dialogue)
    namelist <- c(namelist, name)
  }
  
  all_clean <- all_dialogue %>% 
    VectorSource() %>% 
    VCorpus() %>% 
    cleanCorpus() %>% 
    TermDocumentMatrix() %>%
    as.matrix()
  
  colnames(all_clean) <- namelist
  
  assign("all_clean",all_clean,.GlobalEnv)
  all_clean %>% head()
  
}



#Episode IV: A New Hope

# How many dialogues?
length(ep4$dialogue)

# How many characters?
length(levels(ep4$character))

# Top 20 characters with more dialogues 
top.ep4.chars <- as.data.frame(sort(table(ep4$character), decreasing=TRUE))[1:20,]

# Visualization (bar plot)
pdf(file = "Character by No of Dialogue (Ep4).pdf", width = 8.5, height = 8.5)
  ggplot(data=top.ep4.chars, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity", fill="red2", colour="black") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    ggtitle("Top 20 Characters with Most Number of Dialogues (Episode IV)") + theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="Character", y="Number of dialogues")
dev.off()

# Wordcloud for Episode IV 
# You need to refresh the viewer a couple (or 10) times for the first wordcloud (buggy)
# There is a bug for wordcloud2 from CRAN that only shows the masked image. 
# To Solve: Remove the installed wordcloud2, reinstall using wordcloud2 from GitHub using: devtools::install_github("lchiffon/wordcloud2")
df_IV <- frequentTerms(ep4$dialogue)
pal_IV <- brewer.pal(4,"RdGy")
figPath_IV <- "C:/Users/user/Desktop/Web and network analytics/Coursework/wordcloud_masks/vader.png" #set path of pic
wordcloud2(df_IV, size=0.5, figPath=figPath_IV, color = pal_IV)
# or
df_IV <- frequentTerms(ep4$dialogue)
wordcloud2(df_IV, size=0.5, shape = "star")
# or 
pdf(file = "WordCloud (Ep4).pdf", width = 8.5, height = 8.5)
  df_IV<- frequentTerms(ep4$dialogue)
  wordcloud(df_IV$word, df_IV$freq, max.words=100, color=pal_IV)
dev.off()

# Most frequent bigrams
pdf(file = "Most frequent bigrams (Ep4).pdf", width = 8.5, height = 8.5)
  ep4.bigrams <- frequentBigrams(ep4$dialogue)[1:20,]
  ggplot(data=ep4.bigrams, aes(x=reorder(word, -freq), y=freq)) +  
    geom_bar(stat="identity", fill="firebrick2", colour="black") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    ggtitle("Top 20 Most Frequent Bigrams (Episode IV)") + theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="Bigram", y="Frequency")
dev.off()



#Episode V: The Empire Strikes Back

# How many dialogues?
length(ep5$dialogue)

# How many characters?
length(levels(ep5$character))

# Top 20 characters with more dialogues 
top.ep5.chars <- as.data.frame(sort(table(ep5$character), decreasing=TRUE))[1:20,]

# Visualization (bar plot)
pdf(file = "Character by No of Dialogue (Ep5).pdf", width = 8.5, height = 8.5)
  ggplot(data=top.ep5.chars, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity", fill="green2", colour="black") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1)) + 
    ggtitle("Top 20 Characters with Most Number of Dialogues (Episode V)") + theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="Character", y="Number of dialogues")
dev.off()

# Wordcloud for Episode V
# You need to refresh the viewer a couple (or 10) times for the first wordcloud (buggy)
# There is a bug for wordcloud2 from CRAN that only shows the masked image. 
# To Solve: Remove the installed wordcloud2, reinstall using wordcloud2 from GitHub using: devtools::install_github("lchiffon/wordcloud2")
df_V <- frequentTerms(ep5$dialogue)
pal_V <- brewer.pal(4,"PRGn")
figPath_V <- "C:/Users/user/Desktop/Web and network analytics/Coursework/wordcloud_masks/yoda.png" #set path of pic
wordcloud2(df_V, size=0.5, figPath=figPath_V, color=pal_V)
# or
df_V <- frequentTerms(ep5$dialogue)
wordcloud2(df_V, size=0.5, shape = "star")
# or 
pdf(file = "WordCloud (Ep5).pdf", width = 8.5, height = 8.5)
  df_V <- frequentTerms(ep5$dialogue)
  wordcloud(df_V$word, df_V$freq, max.words=100, color=pal_V)
dev.off()
  
# Most frequent bigrams
pdf(file = "Most frequent bigrams (Ep5).pdf", width = 8.5, height = 8.5)
  ep5.bigrams <- frequentBigrams(ep5$dialogue)[1:20,]
  ggplot(data=ep5.bigrams, aes(x=reorder(word, -freq), y=freq)) +  
    geom_bar(stat="identity", fill="seagreen2", colour="black") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    ggtitle("Top 20 Most Frequent Bigrams (Episode V)") + theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="Bigram", y="Frequency")
dev.off()



#Episode VI: Return of the Jedi

# How many dialogues?
length(ep6$dialogue)

# How many characters?
length(levels(ep6$character))

# Top 20 characters with more dialogues
top.ep6.chars <- as.data.frame(sort(table(ep6$character), decreasing=TRUE))[1:20,]

# Visualization (bar plot)
pdf(file = "Character by No of Dialogue (Ep6).pdf", width = 8.5, height = 8.5)
  ggplot(data=top.ep6.chars, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity", fill="blue2", colour="black") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    ggtitle("Top 20 Characters with Most Number of Dialogues (Episode VI)") + theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="Character", y="Number of dialogues")
dev.off()

# Wordcloud for Episode VI
# You need to refresh the viewer a couple (or 10) times for the first wordcloud (buggy)
# There is a bug for wordcloud2 from CRAN that only shows the masked image. 
# To Solve: Remove the installed wordcloud2, reinstall using wordcloud2 from GitHub using: devtools::install_github("lchiffon/wordcloud2")
df_VI <- frequentTerms(ep6$dialogue)
pal_VI <- brewer.pal(4, "RdBu")
figPath_VI <- "C:/Users/user/Desktop/Web and network analytics/Coursework/wordcloud_masks/r2d2.png" #set path of pic
wordcloud2(df_VI, size=0.5, figPath=figPath_VI, color=pal_VI)
# or
df_VI <- frequentTerms(ep6$dialogue)
wordcloud2(df_VI, size=0.5, shape = "star")
# or 
pdf(file = "WordCloud (Ep6).pdf", width = 8.5, height = 8.5)
  df_VI <- frequentTerms(ep6$dialogue)
  wordcloud(df_VI$word, df_VI$freq, max.words=100, color=pal_VI)
dev.off()

# Most frequent bigrams
pdf(file = "Most frequent bigrams (Ep6).pdf", width = 8.5, height = 8.5)
  ep6.bigrams <- frequentBigrams(ep6$dialogue)[1:20,]
  ggplot(data=ep6.bigrams, aes(x=reorder(word, -freq), y=freq)) +  
    geom_bar(stat="identity", fill="steelblue2", colour="black") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    ggtitle("Top 20 Most Frequent Bigrams (Episode VI)") + theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="Bigram", y="Frequency")
dev.off()




# The Original Trilogy: Episode IV, V, and VI

# How many dialogues?
length(trilogy$dialogue)

# How many characters?
length(levels(trilogy$character))

# Top 20 characters with more dialogues 
top.trilogy.chars <- as.data.frame(sort(table(trilogy$character), decreasing=TRUE))[1:20,]

# Visualization 
# bar chart
pdf(file = "Character by No of Dialogue (Trilogy).pdf", width = 8.5, height = 8.5)
  ggplot(data=top.trilogy.chars, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity", fill="darkorchid2", colour="black") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    ggtitle("Top 20 Characters with Most Number of Dialogues (Trilogy)") + theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="Character", y="Number of dialogues")
dev.off()

# interactive colored treemap
top_chars <- trilogy %>% count(character) %>% arrange(desc(n)) %>% head(20)

hchart(top_chars, type = 'treemap',hcaes(x = "character", value = 'n', color = 'n'))

clean_top_char(trilogy)

# Wordcloud for The Original Trilogy
# You need to refresh the viewer a couple (or 10) times for the first wordcloud (buggy)
# There is a bug for wordcloud2 from CRAN that only shows the masked image. 
# To Solve: Remove the installed wordcloud2, reinstall using wordcloud2 from GitHub using: devtools::install_github("lchiffon/wordcloud2")
df_trilogy <- frequentTerms(trilogy$dialogue)
pal_trilogy <- brewer.pal(4,"Dark2")
figPath_trilogy <- "C:/Users/user/Desktop/Web and network analytics/Coursework/wordcloud_masks/rebel alliance.png"
wordcloud2(df_trilogy, size=0.5, figPath=figPath_trilogy, color=pal_trilogy)
# or
df_trilogy <- frequentTerms(trilogy$dialogue)
wordcloud2(df_trilogy, size=0.5, shape = "star")
# or
pdf(file = "WordCloud (Trilogy).pdf", width = 8.5, height = 8.5)
  df_trilogy <- frequentTerms(trilogy$dialogue)
  wordcloud(df_trilogy$word, df_trilogy$freq, max.words=100, color=pal_trilogy)
dev.off()

# Most frequent bigrams
pdf(file = "Most frequent bigrams (Trilogy).pdf", width = 8.5, height = 8.5)
  trilogy.bigrams <- frequentBigrams(trilogy$dialogue)[1:20,]
  ggplot(data=trilogy.bigrams, aes(x=reorder(word, -freq), y=freq)) +  
    geom_bar(stat="identity", fill="slateblue2", colour="black") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    ggtitle("Top 20 Most Frequent Bigrams (Trilogy)") + theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="Bigram", y="Frequency")
dev.off()

# Commonality cloud between Luke and Vader
pdf(file = "Commonality cloud between Luke and Vader.pdf", width = 8.5, height = 8.5)
  commonality.cloud(all_clean[,c("LUKE","VADER")], colors = "steelblue1", at.least = 2, max.words = 100)
dev.off()

# Comparison cloud between Luke and Vader
pdf(file = "Comparison cloud between Luke and Vader.pdf", width = 8.5, height = 8.5)
  comparison.cloud(all_clean[,c("VADER","LUKE")], colors = c("#F8766D", "#00BFC4"), max.words=50)
dev.off()




# Analysis B: Sentiment analysis

ep4$episode <- 'ep4'
ep5$episode <- 'ep5'
ep6$episode <- 'ep6'

trilogy <- bind_rows(ep4, ep5, ep6)
#trilogy <- rbind(ep4,ep5,ep6)

# SA Part I: Sentiment Cloud Positive and Negative words
# Episode IV Transform the text to a tidy data structure with one token per row
ep4tokens <- ep4 %>%  
  mutate(dialogue=as.character(ep4$dialogue)) %>%
  unnest_tokens(word, dialogue)

# Episode IV Positive and negative words
pdf(file = "Sentiment Cloud (Ep4).pdf", width = 8.5, height = 8.5)
ep4tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors=c("#F8766D", "#00BFC4"), max.words=100)
dev.off()

# Episode V Transform the text to a tidy data structure with one token per row
ep5tokens <- ep5 %>%  
  mutate(dialogue=as.character(ep5$dialogue)) %>%
  unnest_tokens(word, dialogue)

# Episode V Positive and negative words
pdf(file = "Sentiment Cloud (Ep5).pdf", width = 8.5, height = 8.5)
ep5tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors=c("#F8766D", "#00BFC4"), max.words=100)
dev.off()

# Episode VI Transform the text to a tidy data structure with one token per row
ep6tokens <- ep6 %>%  
  mutate(dialogue=as.character(ep6$dialogue)) %>%
  unnest_tokens(word, dialogue)

# Episode VI Positive and negative words
pdf(file = "Sentiment Cloud (Ep6).pdf", width = 8.5, height = 8.5)
ep6tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors=c("#F8766D", "#00BFC4"), max.words=100)
dev.off()

# Trilogy Transform the text to a tidy data structure with one token per row
tritokens <- trilogy %>%  
  mutate(dialogue=as.character(trilogy$dialogue)) %>%
  unnest_tokens(word, dialogue)

# Trilogy Positive and negative words
pdf(file = "Sentiment Cloud (Trilogy).pdf", width = 8.5, height = 8.5)
tritokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors=c("#F8766D", "#00BFC4"), max.words=100)
dev.off()


# SA Part II: See which episode has more p/n words
sent_bing <- tritokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(episode, sentiment, sort = TRUE)

# Converting the dataframe to wide format to get the ratio of positive to negative words in each episode.
sent_bing_wide<-dcast(sent_bing,episode~sentiment, value.var = "n")

# Creating a variable with positive to negative word ratio.
sent_bing_wide%>%
  mutate(PNratio = positive/negative)


# SA Part III: Sentiment analysis (P/N) words ratio for each character in all the episodes
sent_character_bing <- tritokens %>%
  inner_join(get_sentiments("bing"),by="word") %>%
  count(character, sentiment, sort = TRUE) %>%
  dcast(character~sentiment, value.var = "n", fill = -1) %>%
  mutate(PNratio = positive/negative) %>%
  filter(PNratio > 0) %>%
  arrange(desc(PNratio))

# creating line number variable for each line in each episode
tidy_episode <- trilogy %>%
  mutate(dialogue=as.character(trilogy$dialogue)) %>%
  group_by(episode) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, dialogue)

# creating an index variable which defines different subsegments of the episode
tidy_episode_sentiment <- tidy_episode %>%
  mutate(index = linenumber %/% 10)

# Plotting the trend of sentiment flow in each of the star wars episodes
pdf(file = "Sentiment Flow Trend (Trilogy).pdf", width = 8.5, height = 8.5)
tidy_episode_sentiment %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(episode, index, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(net_sentiment = positive - negative) %>%   #finding the difference net sentiment
  ggplot(aes(index, net_sentiment, fill=episode)) +
  geom_col(show.legend = F) +
  facet_wrap(~episode , ncol=2, scales="free_x") + 
  ggtitle("Sentiment flow throughout the episode for different episodes") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Subsegments of the episodes",y="sentiment flow throughout each episode")
dev.off()


# SA Part IV: Sentiments and frequency associated with each word  
sent_nrc <- tritokens %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) 

# Frequency of each sentiment
# Which include postive, negative, trust, anticipation, fear, anger, joy, sadness, surprise, and disgust.
pdf(file = "Sentiment by Frequency (Trilogy).pdf", width = 8.5, height = 8.5)
  ggplot(data=sent_nrc, aes(x=reorder(sentiment, -n, sum), y=n)) + 
    geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
    ggtitle("Frequency of each sentiment") + theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="Sentiment", y="Frequency") +
    theme_bw() 
dev.off()

# Top 10 terms for each sentiment
pdf(file = "Sentiment by Top10 words (Trilogy).pdf", width = 8.5, height = 8.5)
  sent_nrc %>%
    group_by(sentiment) %>%
    arrange(desc(n)) %>%
    slice(1:10) %>%
    ggplot(aes(x=reorder(word, n), y=n)) +
    geom_col(aes(fill=sentiment), show.legend=FALSE) +
    facet_wrap(~sentiment, scales="free_y") +
    labs(y="Frequency", x="Terms") +
    ggtitle("Top 10 terms for each sentiment") + theme(plot.title = element_text(hjust = 0.5)) +
    coord_flip() +
    theme_bw() 
dev.off()
  
  
# SA Part V: Analysis by character 
# Sentiment analysis for the Top 10 characters with the most dialogues
pdf(file = "Sentiment by Top 10 Character with Most Dialogues (Trilogy).pdf", width = 8.5, height = 8.5)
  tritokens %>%
    filter(character %in% c("LUKE","HAN","THREEPIO","LEIA","VADER",
                            "BEN","LANDO","YODA","EMPEROR","RED LEADER")) %>%
    inner_join(get_sentiments("nrc")) %>%
    count(character, sentiment, sort=TRUE) %>%
    ggplot(aes(x=sentiment, y=n)) +
    geom_col(aes(fill=sentiment), show.legend=FALSE) +
    facet_wrap(~character, scales="free_x") +
    ggtitle("Sentiment by Top 10 Character with Most Dialogues") + theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="Sentiment", y="Frequency") +
    coord_flip() +
    theme_bw() 
dev.off()

# Text Analysis for the Top 10 characters with more dialogues
# Stopwords
mystopwords <- data_frame(word=c(stopwords("english"), 
                                 c("thats","weve","hes","theres","ive","im",
                                   "will","can","cant","dont","youve","us",
                                   "youre","youll","theyre","whats","didnt")))

# Tokens without stopwords
top.chars.tokens <- trilogy %>%
  mutate(dialogue=as.character(trilogy$dialogue)) %>%
  filter(character %in% c("LUKE","HAN","THREEPIO","LEIA","VADER",
                          "BEN","LANDO","YODA","EMPEROR","RED LEADER")) %>%
  unnest_tokens(word, dialogue) %>%
  anti_join(mystopwords, by="word")

# Most frequent words for each character
pdf(file = "Sentiment by Character with Most Frequent Words (Trilogy).pdf", width = 8.5, height = 8.5)
  top.chars.tokens %>%
    count(character, word) %>%
    group_by(character) %>% 
    arrange(desc(n)) %>%
    slice(1:10) %>%
    ungroup() %>%
    mutate(word2=factor(paste(word, character, sep="__"), 
                        levels=rev(paste(word, character, sep="__"))))%>%
    ggplot(aes(x=word2, y=n)) +
    geom_col(aes(fill=character), show.legend=FALSE) +
    facet_wrap(~character, scales="free_y") +
    labs(x="Sentiment", y="Frequency") +
    scale_x_discrete(labels=function(x) gsub("__.+$", "", x)) +
    ggtitle("Sentiment by Top 10 Character with Most Frequent Words") + theme(plot.title = element_text(hjust = 0.5)) +
    coord_flip() +
    theme_bw()
dev.off()

# Most relevant words for each character
pdf(file = "Sentiment by Character with Most Relevant Words (Trilogy).pdf", width = 8.5, height = 8.5)
  top.chars.tokens %>%
    count(character, word) %>%
    bind_tf_idf(word, character, n) %>%
    group_by(character) %>% 
    arrange(desc(tf_idf)) %>%
    slice(1:10) %>%
    ungroup() %>%
    mutate(word2=factor(paste(word, character, sep="__"), 
                        levels=rev(paste(word, character, sep="__"))))%>%
    ggplot(aes(x=word2, y=tf_idf)) +
    geom_col(aes(fill=character), show.legend=FALSE) +
    facet_wrap(~character, scales="free_y") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    labs(y="tf-idf", x="Sentiment") +
    scale_x_discrete(labels=function(x) gsub("__.+$", "", x)) +
    ggtitle("Sentiment by Top 10 Character with Most Relevant Words") + theme(plot.title = element_text(hjust = 0.5)) +
    coord_flip() +
    theme_bw()
dev.off()


# SA Part VI: Sentiment Analysis of Luke Vs. Vader 
# Sentiment cloud for Luke SkyWalker
pdf(file = "Sentiment Cloud (Luke SkyWalker).pdf", width = 8.5, height = 8.5)
  all_clean %>%
    as.data.frame() %>% 
    rownames_to_column(var = 'word') %>%
    inner_join(get_sentiments("bing"), by = 'word') %>% 
    select(word, LUKE, sentiment) %>% 
    spread(sentiment, LUKE, fill = 0) %>% 
    column_to_rownames(var = 'word') %>% 
    comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words=150)
dev.off()

# Sentiment cloud for Darth Vader
pdf(file = "Sentiment Cloud (Darth Vader).pdf", width = 8.5, height = 8.5)
  all_clean %>%
    as.data.frame() %>% 
    rownames_to_column(var = 'word') %>%
    inner_join(get_sentiments("bing"), by = 'word') %>% 
    select(word, VADER, sentiment) %>% 
    spread(sentiment, VADER, fill = 0) %>% 
    column_to_rownames(var = 'word') %>% 
    comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words=150)
dev.off()

# Sentiment Comparison between Luke and Vader
pdf(file = "Sentiment Comparison (Luke Vs Vader).pdf", width = 8.5, height = 8.5)
  senti_LUKE_VADER <- all_clean %>%
    as.data.frame() %>% 
    rownames_to_column(var = 'word') %>%
    inner_join(get_sentiments("nrc"), by = 'word')%>% 
    select(LUKE, VADER, sentiment) %>% 
    group_by(sentiment) %>% 
    summarise(sum_luke = sum(LUKE),
            sum_vader = sum(VADER))
  # Plotting the comparison using pyramid plot 
  pyramid.plot(senti_LUKE_VADER$sum_luke, senti_LUKE_VADER$sum_vader,
               labels = senti_LUKE_VADER$sentiment, gap = 40,
               top.labels = c("LUKE", "Sentiment", "VADER"),
               main = "Sentiment Comparison", laxlab = NULL, 
               raxlab = NULL, unit = NULL)
dev.off()






# Analysis C: Word Association and Relationship Networks Analysis

# Master Yoda Word Association Network
# Argument: Focused on the word/string 'jedi'
pdf(file = "Word Association Network (Master Yoda).pdf", width = 8.5, height = 8.5)
word_associate(trilogy$dialogue[trilogy$character == 'YODA'], match.string = c("jedi"), 
               stopwords = c(stopwords("english"), c("thats","weve","hes","theres","ive","im",
                                                     "will","can","cant","dont","youve","us",
                                                     "youre","youll","theyre","whats","didnt")), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkgreen"))
# Add title
title(main = "Master Yoda")
dev.off()

# Darth Vader Word Association Network
# Argument: Focused on the word/string 'rebel'
pdf(file = "Word Association Network (Darth Vader).pdf", width = 8.5, height = 8.5)
word_associate(trilogy$dialogue[trilogy$character == 'VADER'], match.string = c("rebel"), 
               stopwords = c(stopwords("english"), c("thats","weve","hes","theres","ive","im",
                                                     "will","can","cant","dont","youve","us",
                                                     "youre","youll","theyre","whats","didnt")), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
# Add title
title(main = "Darth Vader")
dev.off()

# Star Wars Relationship Network
# Relation Network 1
# The following plot shows the relationships between each character throughout the entire film. 
pdf(file = "Star Wars Relationship Network 1.pdf", width = 8.5, height = 8.5)
char <- colnames(all_clean) %>% tolower()

all_clean_dt <- all_clean %>% 
  as.data.frame() %>% 
  select(-10)

colnames(all_clean_dt) <- colnames(all_clean_dt) %>% tolower()

network <- all_clean_dt[rownames(all_clean) %in% char,]

network_matrix <- network %>% select(rownames(network)) %>% as.matrix()

network1 = graph_from_adjacency_matrix(network_matrix, mode='undirected', diag=F)

plot(network1,                
     vertex.color = rgb(0.8,0.4,0.3,0.8),          # Node color
     vertex.frame.color = "white",                 # Node border color
     vertex.shape="circle",                        # One of "none", "circle", "square", "csquare", "rectangle" "crectangle", "vrectangle", "pie", "raster", or "sphere"
     vertex.size=54,                               # Size of the node (default is 15)
     vertex.size2=NA,                              # The second size of the node (e.g. for a rectangle)
     # Character vector used to label the nodes
     vertex.label.color="white",
     vertex.label.family="serif",                   # Font family of the label (e.g."Times", "Helvetica"), windowsFonts() to check the available fonts in the PC
     vertex.label.font=c(1,2,3,4),                  # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex=c(0.7,1,1.3),                 # Font size (multiplication factor, device-dependent)
     vertex.label.dist=0,                           # Distance between the label and the vertex
     vertex.label.degree=0,                         # The position of the label in relation to the vertex (use pi)ze2=NA)  # The second size of the node (e.g. for a rectangle)
     layout=layout.circle, main="Star Wars Relationship Network")   
dev.off()


# Relation Network 2
# This figure improves the previous relationship network by graphing and combining all the 
#multiple relationships of one character to another into a singular line for easier viewing and interpretation. 
pdf(file = "Star Wars Relationship Network 2.pdf", width = 8.5, height = 8.5)
network2 <- network %>% 
  rownames_to_column('word1') %>% 
  gather(word2, dialogue, -1)

network2 %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = dialogue), show.legend = FALSE) +
  geom_node_point(color = "firebrick", size = 20, alpha = .5) +
  geom_node_text(aes(label = name), col = "white") +
  theme_solarized(light = F)
dev.off()




