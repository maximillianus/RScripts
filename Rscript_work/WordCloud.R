######################### Create WordCloud ############################
rm(list=ls())
# Text mining script : : Written - Pavan 
options(warn=-1)

args<-commandArgs(trailingOnly = T)

#text <- "The non-stop platform which support Australian ATM, EFTPOS & MyTell PinPad systems was life-cycled flawlessly to new infrastructure. Last three days of data shows improvements in some of batch runs, Barry will share more telemetry next week.
#        .POS Credit Extract runs multiple times throughout the day, morning run reduced from 20 mins to 7 mins
#        .POS TANSTAT report run time reduced from around 4.5 hours to 1.5 hours"

require(tm)
require(wordcloud)
require(memoise)
require(syuzhet)

.libPaths()

#suppressMessages(require(tm))
#suppressMessages(require(wordcloud))
#suppressMessages(require(memoise))
#suppressMessages(require(syuzhet))

# text <- "The non-stop platform which support Australian ATM, EFTPOS & MyTell PinPad systems was life-cycled flawlessly to new #infrastructure. Last three days of data shows improvements in some of batch runs, Barry will share more telemetry next week.
#          ·POS Credit Extract runs multiple times throughout the day, morning run reduced from 20 mins to 7 mins   
#          ·POS TANSTAT report run time reduced from around 4.5 hours to 1.5 hours
#         From the Platform Finance board data it's clear that next year we do not have budget uplift compared to D&A and this means we have to continue to drive self-fudded business cases to continue transformation of Platforms. Our culture to challenge cost is visible, I have seen number of business cases from you which are self-funding. Our finance colleagues Julian and Majella have been a great support throughout the year. They have fixed one of our burning issue with wider project accounting community. Depreciating will start for any infrastructure bought by projects once that Capital item is switched on rather than when the project goes live. This helps with the run off issues around refreshing the hardware and maintenance contracts.
#          I would like to thank you for operating with urgency to support Windows and Unix Internal Audits. As part of the preparation for 3 key audits in 2016 across Unix, Windows and Databases, we have self-disclosed to audit the overall ratings across these key platform technologies. This was based on existing risk assessments of these platforms as well as current open audit issues. Major concern for me are Identity and Access Management controls weakness and Known End of Life issues across Platforms. Both areas are complex to solve, are represented as current risks under governance by the Group Technology Risk Forum, and have strategic programs currently being rolled out (e.g. Enterprise Server Upgrade, Privileged Access Management (SEP)). The ratings are also consistent with past audits relating to these platforms though note below significant progress on past audit issues raised (across the platforms there are 8 audit issues outstanding from past audits, with a total of 88 audit issues implemented).
#          Thursday and Friday I spent significant amount of time on working through the Storage migration changes. Nicole gave me a good counsel to raise change with simple language, articulate previous incidents on back of change and mitigation actions or any change in the methodology. We must call this out clearly in the change record, this will avoid dependency on other team to provide you data which may not be related to your change. I have explained to Storage migration team yesterday. I will join Tuesday Platform ELT meeting and give you more update. TBOS incident is not yet recovered, lets focus on recovery before we migrate another similar cluster/payment system.
#          There were series of announcement yesterday from CEO, COO ad CIO office. I understand that any change, however big or small, can create curiosity and I appreciate your unwavering commitment to delivering for our business, our services. Remember, nothing has changed from their perspective and their expectations on us remain. Keep up the great work. Its "business as usual"."


var1 <- as.character(args[1])
print(var1)
tweets <- get_text_as_string(var1)

text <- as.character(tweets)
myCorpus = Corpus(VectorSource(text))

RemovedWordVector <- c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but")
myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords, RemovedWordVector)


myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))

m = as.matrix(myDTM)

word_freqs <- sort(rowSums(m), decreasing = TRUE)
dm = data.frame(word = names(word_freqs), freq = word_freqs)

#print(paste(args[1]))

filepath1 <-  paste(args[2])

jpeg(file = filepath1) 

wordcloud(dm$word, dm$freq,scale=c(4,0.5),random.order = FALSE, colors = brewer.pal(8, "Dark2"))
dev.off()

#wordcloud(dm$word, dm$freq, scale=c(4,0.5),
#              min.freq = 3, max.words=9,
#              colors=brewer.pal(8, "Dark2"))



#wordcloud(names(v), v, scale=c(4,0.5),
#          min.freq = 10, max.words=15,
#          colors=brewer.pal(8, "Dark2"))

rm(list=ls())