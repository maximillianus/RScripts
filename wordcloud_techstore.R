#### WORDCLOUD on TechStore Script ####

## NOTE: Ensure your directory has 'datafiles/' folder with necessary files
## NOTE: This script will produce a wordcloud but not save it to file.
##       Screenshot the resulting image to save wordcloud.
## FILES needed:
##    - Techstore Admin - Cleaned.xlsx
##    - Techstore_comment.html
##    - techstore_cart.png

# Setting working directory to wherever folder this script file is being run
this.dir <- dirname(parent.frame(2)$ofile)
print(this.dir)
setwd(this.dir)

## Load libraries - ensure your system has these libraries
library(openxlsx)
library(dplyr)
library(tidytext)
library(wordcloud2)
library(rvest)

## Read data from excel
#require(openxlsx)
textdata <- read.xlsx('datafiles/TechStore Admin - Cleaned.xlsx')

## Prepare data
textdata <- textdata[,c('FEEDBACK.ID', 'DESCRIPTION')]
textdata <- textdata[order(textdata$FEEDBACK.ID),]
names(textdata) <- tolower(names(textdata))
names(textdata)[1] <- "id"

## Count word
#require(tidytext)
worddata <- textdata %>% unnest_tokens(word, description)

#Stop words
#require(dplyr)
my_stopwords <- c('i', 'you', 'he', 'she', 'we', 'they', 'it',
                  'my', 'your', 'his', 'her', 'our', 'their', 'its',
                  'me', 'you', 'him', 'her', 'us', 'them',
                  'mine', 'yours', 'his', 'hers', 'ours', 'theirs',
                  'a', 'the', 'be', 'is', 'am', 'are', 'to', 'too',
                  'this', 'these', 'that', 'those', 'and', 'for',
                  'on', 'of', 'off', 'as', 'not')
#make into tibble
my_stopwords <- tibble(word=my_stopwords)

#Word Count
techstorewords <- worddata %>% anti_join(my_stopwords) %>% count(word, sort=T)
# make into dataframe format
techstorewords <- data.frame(word = techstorewords$word, freq=techstorewords$n)


## Data from Maxconnect

## Read data from webpage
#require(rvest)
webpage <- read_html("datafiles/Techstore_Comment.html")
comment_data_html <- html_nodes(webpage, '.ngSummary')
comment_data <- html_text(comment_data_html)

## Counting data
#unnest into words
comment_df <- data.frame(line=1:length(comment_data), comment=comment_data, stringsAsFactors = FALSE)
comment_data_tokens <- comment_df %>% unnest_tokens(word, comment)

#Creaing stopwords
my_stopwords <- c('i', 'you', 'he', 'she', 'we', 'they', 'it',
                  'my', 'your', 'his', 'her', 'our', 'their', 'its',
                  'me', 'you', 'him', 'her', 'us', 'them',
                  'mine', 'yours', 'his', 'hers', 'ours', 'theirs',
                  'a', 'the', 'be', 'is', 'am', 'are', 'to', 'too',
                  'this', 'these', 'that', 'those', 'and', 'for',
                  'on', 'of', 'off', 'as', 'not',
                  'on','in','with','will','shall','or', 'but', 'hi')
my_stopwords <- tibble(word=my_stopwords)

# count words
techstorecomment <- comment_data_tokens %>% anti_join(my_stopwords) %>% count(word, sort=T)
names(techstorecomment)[2] <- 'freq'

# Joining techstore excel data & maxconnect comment data
techstore.totalwords <- bind_rows(techstorewords,techstorecomment) %>% group_by(word) %>% summarise_all(sum) %>% ungroup %>% arrange(desc(freq))
techstore.totalwords <- as.data.frame(techstore.totalwords)


## Final wordcloud
 # subset data to reduce words if necessary
#techstore.totalwords <- techstore.totalwords[1:300,]
 # creating color pallete for the text
anzbluecol <- colorRampPalette(c("#498fff", "#001d4c"))

wordcloud2(techstore.totalwords, 
           size=0.9, 
           gridSize = 5,
           fontFamily = 'Calibri', ##Ensure machine has Calibri font
           minRotation = pi/2,
           maxRotation = pi/2,
           fontWeight = 'bold',
           color = rep(anzbluecol(8), nrow(techstore.totalwords)),
           backgroundColor = 'white',
           ellipticity = 0.65,
           rotateRatio = 0.4,
           shuffle = FALSE,
           #widgetsize = c(900, 900),
           figPath = 'datafiles/techstore_cart.png')

## FUTURE: script to save wordcloud image?

######## END WORDCLOUD Script #########