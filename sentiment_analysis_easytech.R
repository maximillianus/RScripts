######## SENTIMENT ANALYSIS on EASYTECH ########

#### Load Library ####
# rm(list=ls())
#.libPaths('C:/Users/pradana1/home/R/win-library/3.3')
require(rvest) ##for reading data
require(tidytext) ## for unnest token
require(dplyr) ## for various summarise, mutate, groupby function
require(tidyr)
require(ggplot2)
require(plotrix) ## for pie3d chart
######################

######## ============================================================ ########

############ READING THE DATA #############

#require(rvest)
webpage <- read_html("downloads/LiveChatwithHamish2.html")

# Comment Data
#comment_data <- webpage %>% html_nodes(".ngSummary") %>% html_text()
comment_data <- webpage %>% html_nodes(".ngNewsFeedContent .ngSummary") %>% html_text()

# Username Data
#user_name <- webpage %>% html_nodes("[class=ngAuthorLink]") %>% html_text()
user_name <- webpage %>% html_nodes(".ngNewsFeedContent [class=ngAuthorLink]") %>% html_text()

# Class Name data
#class_name <- webpage %>% html_nodes(".ngSummary") %>% html_attr("class")
class_name <- webpage %>% html_nodes(".ngNewsFeedContent .ngSummary") %>% html_attr("class")


##########################################


######## ============================================================ ########


################### EasyTech TEXT CLEANING ####################

# make into dataframe
easytech.df <- data.frame(line=1:length(comment_data),
                          classtype=class_name,
                          comment=comment_data,
                          comment_clean = comment_data,
                          username = user_name, 
                          stringsAsFactors = FALSE)

# placeholder for excluding Group Technology member in the questions
# and comment. This is to minimize size of dataframe and faster script.


# assign id to topicstarter (ngSummary)
ngsummary_v <- which(easytech.df$classtype == 'ngSummary')
easytech.df$questionline <- NA
easytech.df$questionid <- NA
i = 1
for(n in ngsummary_v)
{
  easytech.df$questionline[n:length(easytech.df$questionid)] <- n
  easytech.df$questionid[n:length(easytech.df$questionid)] <- i
  i <- i+1
}
rm(i, n)

########### 1. WORDs & PHRASEs REMOVAL #############

# remove @[...] and @(...) and (asked a question)
easytech.df$comment_clean <- gsub("@\\[[^)]*\\]", '', easytech.df$comment_clean)
easytech.df$comment_clean <- gsub("@\\([^)]*\\)", '', easytech.df$comment_clean)
easytech.df$comment_clean <- gsub("(asked a question)", "", easytech.df$comment_clean)

# remove 'lastName, firstName asked a question' for threadstarter
easytech.df[easytech.df$classtype == 'ngSummary',]$comment_clean <- 
  gsub("^\\w+\\W+\\w+", "", 
       easytech.df[easytech.df$classtype == 'ngSummary',]$comment_clean)

# combine 'easy tech' to easytech
#grep('([eE]asy)\\s([tT]ech)', easytech.df$comment_clean, value=T)
easytech.df$comment_clean <- gsub('([eE]asy)\\s([tT]ech)', '\\1\\2', 
                                  easytech.df$comment_clean)

# combine negation words ie. 'less easy' -> 'less_easy', 'not good' -> 'not_good'
#grep('not\\s', easytech.df$comment_clean, ignore.case = T, value=T)
easytech.df$comment_clean <- gsub('(not)\\s', '\\1_', 
                                  easytech.df$comment_clean,
                                  ignore.case = T
                                  )

#grep('\\sless\\s', easytech.df$comment_clean, ignore.case = T, value=T)
easytech.df$comment_clean <- gsub('\\s(less)\\s', ' \\1_', 
                                  easytech.df$comment_clean ,
                                  ignore.case = T
                                  )


#####################################################


#### 2. REMOVE COMMENTS & QUESTIONS UNRELATED TO EASYTECH ####

# 1. Question / TopicStarter ID

# 2. Find which question id is related to easytech
easytech_v <- ngsummary_v[grepl('easytech|(easy tech)', 
                                easytech.df[ngsummary_v,c('comment_clean')],
                                ignore.case = TRUE)]
not_easytech_v <- ngsummary_v[!grepl('easytech|(easy tech)', 
                                     easytech.df[ngsummary_v,c('comment_clean')],
                                     ignore.case = TRUE)]

# 3. Extract comments related to easytech only
easytechcomment <- easytech.df[0,]  #copy format of dataframe
for (n in not_easytech_v)
{
  x <- easytech.df[easytech.df$questionline == n,c("comment_clean")]
  v <- (grep('easytech|(easy tech)', x, ignore.case = T, value=F))
  print(n-1+v)
  easytechcomment <- bind_rows(easytechcomment, easytech.df[n-1+v,])
  
}
rm(n,x,v)

# 4. Exclude non-easytech questions and bind easytech comments
easytech.df.clean <- easytech.df[easytech.df$questionline %in% easytech_v,]
easytech.df.clean <- bind_rows(easytech.df.clean, easytechcomment)


# 5. exclude 'comment_untidy' column & cleanup variables
easytech.df.clean <- within(easytech.df.clean, remove(comment))

# remove unused variables
rm(easytechcomment, ngsummary_v, easytech_v, not_easytech_v)

###########################################################

########### 3. Specific USERNAME REMOVAL #############

# GTS - Group Technology Service users
GTS_username <- c('Krishnan, Manu', 'Chakraborty, Bikash', 
                  'Adhia, Rushikesh', 'Patel, Pankaj',
                  'Mulla Ashfak')

easytech.df.clean <- easytech.df.clean %>% filter(!username %in% GTS_username)

rm(GTS_username)
######################################################

############### END EasyTech Text Cleaning ###############



## CHECKPOINT: Save dataframe in current working dir
write.csv(easytech.df.clean, 'easytech_comment_clean.csv')


######## ============================================================ ########

################# MEASURING SENTIMENTS ##################


######## CUSTOMIZING THE SENTIMENTS DF/TIBBLE ########

## 1. Sentiments from all lexicon
# Only positive & negative sentiments are considered
#my_sentiment <- filter(sentiments, sentiment == 'positive' | sentiment == 'negative')
#my_sentiment <- unique(my_sentiment)
## CHECK for words which are in both positive and negative sentiment
#ambiguous_words <- my_sentiment %>% count(word) %>% filter(n>1)


## 2. Get only sentiments from Bing
# my_sentiment <- filter(sentiments, lexicon == 'bing')
# my_sentiment <- my_sentiment[,1:2]

## 3. List of positive and negative words for EasyTech
pos_words <- c('good', 'effective','fix','fixed', 'easy','commend','kind',
               'productive','great','brilliant','works','thank','thanks',
               'love','loves','loving', 'help',
               'empowered')
neg_words <- c('annoying','inappropriate', 'disruptive', 'issue', 'irritating',
               'wrong','irony','incessant','rid', 'unintuitive', 'endless',
               'pop','pops','popping', 'nothing', 'intrusive', 'rid',
               'fall','falls','crash','crashes','bog','bogging','bogged')
pos_words_len <- length(pos_words)
neg_words_len <- length(neg_words)
my_sentiment2 <- tibble(word = c(pos_words, neg_words),
                        sentiment = c(rep('positive', pos_words_len), 
                                      rep('negative', neg_words_len) 
                                      )
                        )
rm(pos_words_len, neg_words_len, pos_words, neg_words)

## 4. Custom words to exclude or include
## 4a. Exclude (if necessary)
# excludeword <- c('technology', 'word', 'question' ,'easy', 'less', 'but', 'improvement')
# my_sentiment <- my_sentiment %>% filter(!word %in% excludeword)
# my_sentiment <- my_sentiment %>% filter(!word %in% ambiguous_words$word)
# 
# ## 4b. Include
# includeword <- c("can't", "cant", "cannot", "dont", "don't", "doesnt", "doesn't")
# custom_words <- data_frame(word = includeword,
#                            sentiment = rep('negative', length(includeword) )
#                            )
# my_sentiment <- bind_rows(custom_words, my_sentiment)

## *. Adding negation words: no, not, never, without, less
  #create the dataframe of negation words first
 sentiment_w_less <- my_sentiment2 %>% 
   mutate(word = paste('less', my_sentiment2$word, sep='_') ) %>% 
   mutate(sentiment = ifelse(sentiment == 'positive', 'negative','positive'))
 
 sentiment_w_not <- my_sentiment2 %>% 
   mutate(word = paste('not', my_sentiment2$word, sep='_') ) %>% 
   mutate(sentiment = ifelse(sentiment == 'positive', 'negative','positive'))
 
 #bind rows to original my_sentiment table
 my_sentiment2 <- bind_rows(my_sentiment2, sentiment_w_less)
 my_sentiment2 <- bind_rows(my_sentiment2, sentiment_w_not)
 
 rm(sentiment_w_less, sentiment_w_not)
my_sentiment2 <- unique(my_sentiment2)
 my_sentiment2
 
########################################################


######## 2. JOINING SENTIMENTS IN EASYTECH COMMENT ########

######## METHOD 1: Ranking Sentiments based on words per pararaph ########

## 1. Unnest from paragraph -> words
# easytech.word <- easytech.df.clean %>% unnest_tokens(word, comment_clean)
# 
# ## 2. Join easytech comment's words with sentiment word reference
# easytech.sentiment <- easytech.word %>% inner_join(my_sentiment)
# easytech.sentiment[150:200,c('word','sentiment','username')]
# 
# ## 3. Give +1/-1 value to positive or negative sentiment
# worddata_sentiment$value <- NULL
# worddata_sentiment$value <- ifelse(worddata_sentiment$sentiment == 'positive', 1, -1)

######################### END METHOD 1 ##################################


######## METHOD 2: Ranking Sentiments based on sentences per pararaph ########

## *. Unnest from paragraph -> sentences
easytech.sentence <- easytech.df.clean %>% 
                      unnest_tokens(sentence, comment_clean, token='sentences') %>% 
                      group_by(line) %>% 
                      mutate(sentenceid = row_number()) %>% 
                      ungroup()  
#easytech.sentence

## *. Unnest from sentences -> words
easytech.word <- easytech.sentence %>% 
            unnest_tokens(word, sentence, token = 'words')
#easytech.word


## *. Gauge sentiment PER SENTENCE based on words
easytech.sentiment <- easytech.word %>% inner_join(my_sentiment2)
#easytech.sentiment

## *. Give +1/-1 value PER SENTENCE
easytech.sentiment$value <- NULL
easytech.sentiment$value <- ifelse(easytech.sentiment$sentiment == 'positive', 1, -1)


## *. Calculate sentiment value of sentence
easytech.sentiment.final <- 
  
  easytech.sentiment %>% 
  group_by(line, classtype, username, questionline, questionid, sentenceid) %>% 
  summarise(value_sentence=ifelse(sum(value)>0,1,-1)) %>% 
  group_by(classtype, username, questionline, questionid,line) %>% 
  summarise(value_line=sum(value_sentence))

easytech.df.clean.final <- inner_join(x=easytech.df.clean, y=easytech.sentiment.final[,c('line','value_line')], by='line')
dim(easytech.df.clean.final)
#do this only for repeated pattern. specific for easytech only
easytech.df.clean.final <- easytech.df.clean.final[-10:-11,] 


## Cleanup variables
# rm(easytech.sentiment, easytech.word, 
#    easytech.sentence, easytech.df, 
#    easytech.df.clean)
########################## END METHOD 2 #####################################


write.csv(easytech.df.clean.final, 'easytech_comment_value.csv')



######## METHOD 3: Ranking Sentiments based on word's sentiment value ########
########################### END METHOD 3 #####################################



###########################################################


#### N-gram ####
# easytech.word <- easytech.df.clean %>% 
#   unnest_tokens(bigram, comment_clean,token = 'ngrams', n=2) %>% 
#   separate(bigram, c('word1', 'word2'), sep = " ")
# 
# easytech.word %>% filter(word1 %in% c('not','without','never')) %>% 
#   count(word1, word2, sort=T)

###############


######## 2. CALCULATE SENTIMENT RANK IN EASYTECH  ########

# # calculate sentiment by ID. NOTE: maybe next time should preserve row number (row.names)
# # logic: add up all sentiment then divide by number of sentiment 
# sentiment_value <- tapply(worddata_sentiment$value, worddata_sentiment$line, mean)
# #create into dataframe
# sentiment_value <- data.frame(line=names(sentiment_value), value = sentiment_value)
# #join with original textdata dataframe
# easytech_sv <- merge(easytech.df.clean,sentiment_value, by='line')

#########################################################



######## ============================================================ ########


############# VISUALIZATION #############



## visualizing easytech.df.clean (more refined sentiment analysis)
easytech.df.clean.final  %>% ggplot(aes(line, value_line, fill=value_line > 0)) + 
  geom_col(show.legend = F) +
  xlab("Line ID") + ylab("Sentiment Value") +
  ggtitle("Sentiment Analysis")

#Pie chart
require(plotrix)
slices <- c(sum(easytech.df.clean.final$value_line>0), sum(easytech.df.clean.final$value_line<0))
lbls <- paste0(c('Positive', 'Negative'), ' (', round(slices/sum(slices)*100, 2),'%)')
pie3D(x = slices,
      labels = lbls,
      main = 'Sentiment Ratio',
      col = c('#77dd77', 'salmon'))
rm(lbls, slices)

# #bar plot
# require(ggplot2)
# easytech_sv  %>% ggplot(aes(line, value, fill=value > 0)) + 
#   geom_col(show.legend = F) +
#   xlab("Line ID") + ylab("Sentiment Value") +
#   ggtitle("Sentiment Analysis")
# 
# #Pie chart
# slices <- c(sum(easytech_sv$value>0.3), sum(easytech_sv$value<=0.3))
# lbls <- paste0(c('Positive', 'Negative'), ' (', round(slices/sum(slices)*100, 2),'%)')
# pie3D(x = slices,
#       labels = lbls,
#       main = 'Sentiment Ratio',
#       col = c('#77dd77', 'salmon'))

#########################################


############ END SENTIMENT ANALYSIS ############
