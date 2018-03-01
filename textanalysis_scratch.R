#### TEXT ANALYSIS SCRATCH SCRIPT ####


#### Scratching on TECHSTORE - USER FEEDBACK ####

##Set environment
.libPaths('C:/NotBackedUp/aditya/R/win-library/3.4')
setwd("C:/NotBackedUp/aditya/")

## Load libraries
library(openxlsx)
library(dplyr)
library(tidytext)
library(wordcloud)


## Read Data
require(openxlsx)
textdata <- read.xlsx('datafiles/TechStore Admin.xlsx')

## Read spell-checked data
require(openxlsx)
textdata <- read.xlsx('datafiles/TechStore Admin - Cleaned.xlsx')

## Sort the data based on ID
textdata <- textdata[order(textdata$FEEDBACK.ID),]

## Subsetting Data
textdata <- textdata[,c('FEEDBACK.ID', 'DESCRIPTION')]
names(textdata) <- tolower(names(textdata))
names(textdata)[1] <- "id"

##splitting sentence into words. Output a list based on no of sentence.
require(stringr)
listofwords <- str_match_all(textdata[,2], "\\w+\\b")

require(tidytext)
worddata <- textdata %>% unnest_tokens(word, description)
str(worddata)

# split into sentence
sentencedata <- textdata %>% unnest_tokens(sentence, description, token = 'sentences')

# attach(worddata)
# worddata[worddata$FEEDBACK.ID==16,]$word
# detach(worddata)


#################### ************************************ ######################

## Cleaning Data

 # Check typo in a vector of words
require(hunspell)
hunspell_check(worddata[worddata$FEEDBACK.ID==16,]$word)
 # To find typo words among a vector of sentence:
bad_words <- hunspell_find(textdata[,2])
hunspell_suggest(bad_words[[1]])
bad_words


## Scratching

 #Stop words
require(dplyr)
 #common stopwords
my_stopwords <- c('i', 'you', 'he', 'she', 'we', 'they', 'it',
                  'my', 'your', 'his', 'her', 'our', 'their', 'its',
                  'me', 'you', 'him', 'her', 'us', 'them',
                  'mine', 'yours', 'his', 'hers', 'ours', 'theirs',
                  'a', 'the', 'be', 'is', 'am', 'are', 'to', 'too',
                  'this', 'these', 'that', 'those', 'and', 'for',
                  'on', 'of', 'off', 'as', 'not')
 #anz techstore stopwords
my_stopwords <- c(my_stopwords, 'techstore')
 
my_stopwords <- tibble(word=my_stopwords)

 #Excluding stop words
#worddata %>% anti_join(my_stopwords)

 #Word Count
keywords <- worddata %>% anti_join(my_stopwords) %>% count(word, sort=T)

 #Word Count excluding stopwords from main stopwords dict
keywords <- worddata %>% anti_join(stop_words) %>% count(word, sort=T)

 #WordCloud

require(wordcloud)
wordcloud(words = keywords$word, 
          freq = keywords$n, 
          colors = brewer.pal(8, "Set2"),
          random.order = FALSE,
          #random.color = TRUE,
          min.freq=2)

# x is dataframe of keywords with word + freq column
x <- data.frame(word = keywords$word, freq=keywords$n)
x <- x[1:170,]
x[x$word=='techstore',]$freq = 10
anzbluecol <- colorRampPalette(c("#498fff", "#001d4c"))
whitegreycol <- colorRampPalette(c("#ffffff", "#e8e8e8"))
wordcloud2(x, 
           size=0.65, 
           gridSize = 4,
           fontFamily = 'Calibri',
           minRotation = pi/2,
           maxRotation = pi/2,
           fontWeight = 'bold',
           color = rep(anzbluecol(20), nrow(x)),
           #color = rainbow(1000),
           #backgroundColor = '#007CC1',
           backgroundColor = 'white',
           ellipticity = 0.65,
           rotateRatio = 0.5,
           shuffle = FALSE,
           widgetsize = c(800, 800),
           figPath = 'Cart_techstore.png')

#Dark BG, Light color
wordcloud2(x, 
           size=0.55, 
           gridSize = 3,
           fontFamily = 'Calibri',
           minRotation = pi/2,
           maxRotation = pi/2,
           fontWeight = 'bold',
           color = rep(whitegreycol(10), nrow(x)),
           #color = rainbow(1000),
           backgroundColor = '#007DC1',
           ellipticity = 0.65,
           rotateRatio = 0.5,
           shuffle = FALSE,
           widgetsize = c(700, 700),
           figPath = 'Cart_techstore.png')

 #Spell Checking
misspell <- hunspell_find(textdata[,2])



## Ways to calculate sentiment
# 1. measure number of positive sentence again negative sentence in 1 comment
# 2. count number of positive words vs negative words in 1 comment then plus-minus then calculate the percentage.
# 3. give numeric value to each of positive & negative words and sum up the final
#    value for each comment.

 #Get sentiments from Bing, NRC, Loughran
 #Only positive & negative sentiments are considered
my_sentiment <- filter(sentiments, sentiment == 'positive' | sentiment == 'negative')
my_sentiment <- my_sentiment[,1:2]
my_sentiment <- unique(my_sentiment)

 #Add words into the sentiment dataframe
 # Add: cant, dont
bind_rows(data_frame(word=c('can\'t'), sentiment = c('negative')), my_sentiment)
custom_words <- data_frame(word = c("can't", "cant", "cannot", "dont", "don't"),
                           sentiment = rep('negative',5))

my_sentiment <- bind_rows(custom_words, my_sentiment)

 #Ready to calc sentiment. source = worddata, sentiment = my_sentiment.
worddata_sent <- worddata %>% inner_join(my_sentiment) 
# Result:
# id          word sentiment
# 1   50        faster  positive
# 2   50      friendly  positive
# 3   49    impressive  positive
# 4   48    impressive  positive

worddata_sent$value <- NULL
worddata_sent$value <- ifelse(worddata_sent$sentiment == 'positive', 1, -1)

# calculate sentiment by ID. NOTE: maybe next time should preserve row number (row.names)
# logic: add up all sentiment then divide by number of sentiment 
sentiment_value <- tapply(worddata_sent$value, worddata_sent$id, mean)
 #create into dataframe
sentiment_value <- data.frame(id=names(sentiment_value), value = sentiment_value)
 #join with original textdata dataframe
textdata_sv <- merge(textdata,sentiment_value, by='id')

## Visualization
require(ggplot2)
textdata_sv  %>% ggplot(aes(id, value, fill=value > 0)) + 
                 geom_col(show.legend = F) +
                  xlab("Comment ID") + ylab("Sentiment Value") +
                  ggtitle("Sentiment Analysis") +
                  coord_flip()

## Pie chart
slices <- c(sum(textdata_sv$value>0), sum(textdata_sv$value<0))
lbls <- paste0(c('Positive', 'Negative'), ' (', round(slices/sum(slices)*100, 2),'%)')
pie3D(x = slices,
      labels = lbls,
      main = 'Sentiment Ratio',
      col = c('#77dd77', 'salmon'))
#### END Scratching on TECHSTORE - USERFEEDBACK ####


#### Scratching on TECHSTORE - MaxConnect User Comment ####
require(rvest)
webpage <- read_html("Techstore_Comment.html")
comment_data_html <- html_nodes(webpage, '.ngSummary')
comment_data <- html_text(comment_data_html)

require(tidytext)
comment_df <- data.frame(line=1:length(comment_data), comment=comment_data, stringsAsFactors = FALSE)
comment_data_tokens <- comment_df %>% unnest_tokens(word, comment)

my_stopwords <- c('i', 'you', 'he', 'she', 'we', 'they', 'it',
                  'my', 'your', 'his', 'her', 'our', 'their', 'its',
                  'me', 'you', 'him', 'her', 'us', 'them',
                  'mine', 'yours', 'his', 'hers', 'ours', 'theirs',
                  'a', 'the', 'be', 'is', 'am', 'are', 'to', 'too',
                  'this', 'these', 'that', 'those', 'and', 'for',
                  'on', 'of', 'off', 'as', 'not',
                  'on','in','with','will','shall','or', 'but', 'hi')
my_stopwords <- tibble(word=my_stopwords)

##
fbcount <- x
fbcount$word <- as.character(fbcount$word)

#Word Count
commentcount <- comment_data_tokens %>% anti_join(my_stopwords) %>% count(word, sort=T)
names(commentcount)[2] <- 'freq'

# Joining techstore database data & maxconnect comment data
totalcount <- bind_rows(fbcount,commentcount) %>% group_by(word) %>% summarise_all(sum) %>% ungroup %>% arrange(desc(freq))
totalcount <- as.data.frame(totalcount)

## Visualization - wordcloud
totalcount <- totalcount[1:300,]
wordcloud2(totalcount, 
           size=0.9, 
           gridSize = 5,
           fontFamily = 'Calibri',
           minRotation = pi/2,
           maxRotation = pi/2,
           fontWeight = 'bold',
           color = rep(whitegreycol(10), nrow(x)),
           #color = rainbow(1000),
           backgroundColor = '#007DC1',
           ellipticity = 0.65,
           rotateRatio = 0.4,
           shuffle = FALSE,
           #widgetsize = c(900, 900),
           figPath = 'techstore_cart.png')

# Light BG, dark text
anzbluecol <- colorRampPalette(c("#007cc1", "#005787"))
wordcloud2(totalcount, 
           size=0.9, 
           gridSize = 5,
           fontFamily = 'Calibri',
           minRotation = pi/2,
           maxRotation = pi/2,
           fontWeight = 'bold',
           color = rep(anzbluecol(8), nrow(x)),
           #color = rainbow(1000),
           #backgroundColor = '#007CC1',
           backgroundColor = 'white',
           ellipticity = 0.65,
           rotateRatio = 0.4,
           shuffle = FALSE,
           #widgetsize = c(900, 900),
           figPath = 'techstore_cart.png')

#### END Scratching on TECHSTORE - MaxConnect User Comment ####



#### Scratching on EasyTech - MaxConnect User Comment ####
require(rvest)
webpage <- read_html("Easytech_comment.html")

# Comment Data
comment_html <- webpage %>% html_nodes(".ngSummary")
comment_data <- html_text(comment_html)

# Username Data
# user_name_html <- html_nodes(webpage, '.ngAuthorLink')
# user_name_exclude_list <- grep('ngAuthorLikeLink', html_attrs(user_name_html))
# user_name_html <- user_name_html[-user_name_exclude_list]
# user_name <- html_text(user_name_html)

# Alternative
user_name_html <- webpage %>% html_nodes("[class=ngAuthorLink]")
user_name <- html_text(user_name_html)

# Class Name data
class_name <- comment_html %>% html_attr("class")


#### EasyTech Text Cleaning ####

# make into dataframe
easytech.df <- data.frame(line=1:length(comment_data),
                          classtype=class_name,
                          comment=comment_data, 
                          username = user_name, 
                          stringsAsFactors = FALSE)


easytech.df$comment_clean <- easytech.df$comment
#remove @[...] and @(...)
easytech.df$comment_clean <- gsub("@\\[[^)]*\\]", '', easytech.df$comment_clean)
easytech.df$comment_clean <- gsub("@\\([^)]*\\)", '', easytech.df$comment_clean)
easytech.df$comment_clean <- gsub("(asked a question)", "", easytech.df$comment_clean)

#remove 'lastName, firstName asked a question' for threadstarter
#comment_data_clean <- gsub("^\\w+\\W+\\w+ (asked a question)", "", comment_data_clean)
easytech.df[easytech.df$classtype == 'ngSummary',]$comment_clean <- 
  gsub("^\\w+\\W+\\w+", "", 
       easytech.df[easytech.df$classtype == 'ngSummary',]$comment_clean)

write.csv(easytech.df, 'easytech_comment_clean.csv')

gts_username <- c('Krishnan, Manu', 'Chakraborty, Bikash', 'Adhia, Rushikesh', 'Patel, Pankaj')

easytech.df.clean <- easytech.df %>% filter(!username %in% gts_username)

write.csv(easytech.df.clean, 'easytech_comment_clean.csv')



# Remove comments and questions unrelated to easytech
# if the ngSummary contain easytech or 'easy tech', retain it.
# if the ngSummary does not contain easytech or 'easy tech',
# loop to look for the comments below it.

easytech.df.clean <- easytech.df.clean[-1:-8,]
easytech.df.clean <- easytech.df.clean[-41:-62,]

 # Get sentiments from Bing, NRC, Loughran
 # Only positive & negative sentiments are considered
my_sentiment <- filter(sentiments, sentiment == 'positive' | sentiment == 'negative')
my_sentiment <- my_sentiment[,1:2]
my_sentiment <- unique(my_sentiment)

excludeword <- c('technology', 'word', 'question' ,'easy', 'less', 'but', 'improvement')
my_sentiment <- my_sentiment %>% filter(!word %in% excludeword)

 # Add words into the sentiment dataframe
 # Add: cant, dont
bind_rows(data_frame(word=c('can\'t'), sentiment = c('negative')), my_sentiment)
custom_words <- data_frame(word = c("can't", "cant", "cannot", "dont", "don't", "doesnt", "doesn't"),
                           sentiment = rep('negative',7))

my_sentiment <- bind_rows(custom_words, my_sentiment)

 #Unnest into words
easytech.word <- easytech.df.clean %>% unnest_tokens(word, comment)

worddata_sentiment <- easytech.word %>% inner_join(my_sentiment)

worddata_sentiment$value <- NULL
worddata_sentiment$value <- ifelse(worddata_sentiment$sentiment == 'positive', 1, -1)

easytech.df.clean <- within(easytech.df.clean, remove(comment))
## N-gram
easytech.word <- easytech.df.clean %>% 
  unnest_tokens(bigram, comment_clean,token = 'ngrams', n=2) %>% 
  separate(bigram, c('word1', 'word2'), sep = " ")

easytech.word %>% filter(word1 %in% c('not','without','never')) %>% 
  count(word1, word2, sort=T)




# calculate sentiment by ID. NOTE: maybe next time should preserve row number (row.names)
# logic: add up all sentiment then divide by number of sentiment 
sentiment_value <- tapply(worddata_sentiment$value, worddata_sentiment$line, mean)
#create into dataframe
sentiment_value <- data.frame(line=names(sentiment_value), value = sentiment_value)
#join with original textdata dataframe
easytech_sv <- merge(easytech.df.clean,sentiment_value, by='line')

## Visualization
require(ggplot2)
easytech_sv  %>% ggplot(aes(line, value, fill=value > 0)) + 
  geom_col(show.legend = F) +
  xlab("Line ID") + ylab("Sentiment Value") +
  ggtitle("Sentiment Analysis")
  

slices <- c(sum(easytech_sv$value>0.3), sum(easytech_sv$value<=0.3))
lbls <- paste0(c('Positive', 'Negative'), ' (', round(slices/sum(slices)*100, 2),'%)')
pie3D(x = slices,
      labels = lbls,
      main = 'Sentiment Ratio',
      col = c('#77dd77', 'salmon'))

## Visualization - Wordcloud

## Count the words first:
my_stopwords <- c('i', 'you', 'he', 'she', 'we', 'they', 'it',
                  'my', 'your', 'his', 'her', 'our', 'their', 'its',
                  'me', 'you', 'him', 'her', 'us', 'them',
                  'mine', 'yours', 'his', 'hers', 'ours', 'theirs',
                  'a', 'the', 'be', 'is', 'am', 'are', 'to', 'too',
                  'this', 'these', 'that', 'those', 'and', 'for', 'or',
                  'on', 'of', 'off', 'as', 'not','in','up','but', 'with',
                  'has','have','had','no','not',
                  'patel', 'pankaj', 'adhia', 'rushikesh', 'vinay','adsule',
                  'manu','krishnan','ashfak', 'mulla')
#make into tibble
my_stopwords <- tibble(word=my_stopwords)
easytech.wordcount <- easytech.word %>% anti_join(my_stopwords) %>% count(word, sort=T)
easytech.wordcount <- data.frame(word=easytech.wordcount$word, freq=easytech.wordcount$n, stringsAsFactors = F )

#wordcloud
anzbluecol <- colorRampPalette(c("#007cc1", "#005787"))
wordcloud2(totalcount, 
           size=0.9, 
           gridSize = 5,
           fontFamily = 'Calibri',
           minRotation = pi/2,
           maxRotation = pi/2,
           fontWeight = 'bold',
           color = rep(anzbluecol(8), nrow(x)),
           #color = rainbow(1000),
           #backgroundColor = '#007CC1',
           backgroundColor = 'white',
           ellipticity = 0.65,
           rotateRatio = 0.4,
           shuffle = FALSE,
           #widgetsize = c(900, 900),
           #figPath = 'techstore_cart.png'
           NULL)

require(RColorBrewer)
wordcloud2(easytech.wordcount, 
           shape='circle',
           size=0.65, grid=4,
           color = rep(brewer.pal(8, "Set1"), nrow(easytech.wordcount)),
           minRotation = pi/2,
           maxRotation = pi/2,
           rotateRatio = 0.3,
           fontFamily = 'Calibri',
           shuffle=FALSE)



### HELPER Regular expression ###
# removing the @tag string '@[yourtexthere]'
# Regex exp: started with '@', followed by bracket \\[,
#            followed by any text 0 or more times,
#            ended by another bracket.
gsub("@\\[.*\\]", '', sampletext)

# removing @tag string for name '@(lastname, firstname)'
gsub("@\\(.+\\)", '', sampletext)

# removing lastname, firstname string. Example: 'Doe, John' asked a question
# Regex exp: started with any word/character, repeated several times, 
#            until meet a ',' , then repeat it until meet any word.
gsub(".+\\,.\\S+", "#", sampletext)
gsub("^\\w+\\W+\\w+ (asked a question)", "", sampletext)

# removing 'Doe, John asked a question'
gsub(".+\\,.\\S+ (asked a question)", "#", sampletext)

# replacing standalone 'not' word with combined negated words
# example: I am not happy --> I am not_happy
gsub('\\s(not)\\s', ' \\1_', yourstring, ignore.case = TRUE)
      #\\s(not)\\s means finding ' not ' string
      #replacement \\1_ means replacing with word in parentheses (not) plus '_'
gsub('\\s(not|never|without)\\s', ' \\1_', yourstring, ignore.case = TRUE)
      # same thing with alternation

#### END Scratching on EasyTech - MaxConnect User Comment ####


