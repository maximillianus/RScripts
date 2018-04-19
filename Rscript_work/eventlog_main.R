on#### EVENT LOG Main File ####

## Run this from my own computer first for now

######## Initialize ########
rm(list=ls())
#setwd("\\\\SVRSG001RPS01.asia.corp.anz.com\\pradana1$\\My Documents\\SGWS-8467837A_notbackedup")
setwd("C:/NotBackedUp/")
getwd()
#.libPaths('C:/NotBackedUp/R/win-library/3.4.1')
#options(browser = "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe")
############################

######## Library ########
#require(ggplot2)
#require(googleVis)

#########################


####################### READ DATA ########################

sourcedir <- "\\\\SVRSG001RPS01.asia.corp.anz.com\\pradana1$\\My Documents\\SGWS-8467837A_notbackedup\\Rscript"
folderloc <- "datafiles/eventlogdata/Test/"
filename <- "AUSP-4F935F46_mckenzih_CompiledLog.csv"
fileloc <- paste0(folderloc,filename)

source(paste0(sourcedir,"/eventlog_readdata.R"))
df <- eventlog_readdata(fileloc)

###########################################################


####################### CLEAN DATA ########################

source(paste0(sourcedir,"/eventlog_cleandata.R"))
cleandf <- eventlog_cleandata(df)

## Subset Data
cleandf <- subset(cleandf, !(EventID %in% c(1000)))

cleandf <- subset(cleandf, Date > max(Date)-60)



###########################################################

################## Data Reference ####################

eventsource <- unique(cleandf[,c('EventID','EventSource', 'EventTypeName')])
eventsource <- eventsource[order(eventsource$EventID),]
eventtypecolor <- data.frame(EventTypeName = c('Success','Information', 'Warning', 'Error', 'Critical'),
                             EventColor = c('green', 'blue', 'yellow', 'salmon', 'red')
)
eventsource <- merge(eventsource,eventtypecolor, by="EventTypeName", sort=FALSE)


eventsource_withDesc <- merge(eventsource,cleandf[c('EventID','Description')],by="EventID", sort=FALSE)
eventsource_withDesc <- unique(eventsource_withDesc)
write.csv(eventsource_withDesc, paste0(folderloc,'EventIDRef_',filename))

rm(eventtypecolor)

######################################################



#################### Event Sequencing #####################

## Returning list object containing
# 1. dataframe of detailed event sequence
# 2. dataframe of event sequence in columns
source(paste0(sourcedir,"/eventlog_sequence.R"))
listobj <- eventlog_sequence(cleandf)
main_detailseq_df <- listobj$df1
main_seq_df <- listobj$df2

###########################################################

########## Write Event Sequence Data into file ############

filenametowrite_detailseq <- paste0(folderloc,'DetailSequence_',filename)
filenametowrite_idseq <- paste0(folderloc,'IDsequence_',filename)
#write.csv(main_detailseq_df, filenametowrite_detailseq, row.names=FALSE)
#write.csv(main_seq_df, filenametowrite_idseq, row.names=FALSE)
rm(filenametowrite_detailseq,filenametowrite_idseq)

###########################################################

##################### Sankey Diagram ######################

#main_seq_df <- read.csv("datafiles/eventlogdata/IDsequence_MyApplication.csv")
source(paste0(sourcedir,"/eventlog_sankey.R"))
eventlog_sankey(main_seq_df)



###########################################################

##################### Apriori Rules ######################

#### Subset 1 event ID to check apriori
#### Choose eventID 113 from HyperV0093

singleEventID <- c(4,7011)
mainseqdf_single <- subset(main_seq_df, `0` %in% singleEventID)
rm(singleEventID)

source(paste0(sourcedir,"/eventlog_apriori.R"))
eventlog_apriori(mainseqdf_single)

##########################################################

##################### Basic Event Analysis ######################
## Most common EventID
mostcommonevent <- sort(table(cleandf$EventID), decreasing=T)
par(las=1)
barplot(mostcommonevent[1:10],
        main = "Most Common Event IDs",
        xlab = "Event ID",
        ylab = "Frequency",
        col = "lightblue",
        cex.names=0.9
)


## Most common EventID error
mostcommoneventerror <- sort(table(cleandf[cleandf$EventTypeName=='Error',]$EventID), decreasing=T)
par(las=1)
barplot(mostcommoneventerror[1:10],
        main = paste0("Most Common Error Event IDs\n",filename),
        xlab = "Event ID",
        ylab = "Frequency",
        col = "salmon",
        cex.names=0.8
)

## Most common EventSource error
mostcommoneventerror <- sort(table(cleandf[cleandf$EventTypeName=='Error',]$EventSource), decreasing=T)
par(las=2)
bp <- barplot(mostcommoneventerror[1:10],
        main = paste0("Most Common Error EventID-Source\n",filename),
        #xlab = "Event ID",
        ylab = "Frequency",
        axisnames = FALSE,
        col = "salmon",
        cex.names=0.7
)
labels = names(mostcommoneventerror)[1:10]
text(bp, labels=labels, 
     srt=55, xpd=TRUE,
     cex=0.65, par('usr')[3]-0.25,
     adj=c(1.1,1.1))


## EventID qty by days
eventidbydays <- tapply(cleandf$EventID, list(cleandf$Date), length)
par(las=2)
barplot(eventidbydays,
        main = "Event IDs by Days",
        xlab = "Days",
        ylab = "Frequency",
        col = "lightblue",
        cex.names=0.8
)

## EventID qty by days segmented by Event Type
eventidbydays <- table(cleandf$EventTypeName, cleandf$Date)
par(las=2)
barplot(eventidbydays,
        main = paste0("Daily Event IDs (by Event Type)\n",filename),
        #xlab = "Days",
        ylab = "Frequency",
        col = c("salmon","skyblue", "lightgreen", "yellow"),
        cex.names=0.75
)
legend("topleft", 
       legend = sort(unique(cleandf$EventTypeName)),
       #col = c("salmon","skyblue", "lightgreen", "yellow"),
       fill=c("salmon","skyblue", "lightgreen", "yellow"),
       cex=0.75,
       bty='n',
       y.intersp = 0.7,
       x.intersp = 0.7,
       ncol=2)


## Error qty by days
cleandf_erroronly <- subset(cleandf, EventTypeName == 'Error')
erroridbydays <- table(cleandf$EventID, cleandf$Date)
rm(cleandf_erroronly)
par(las=2)
barplot(erroridbyhours,
        main = "Error IDs by Hours",
        xlab = "Hours",
        ylab = "Frequency",
        col = "lightblue",
        cex.names=0.8
)

## EventID qty by hours
eventidbyhours <- tapply(cleandf$EventID, list(cleandf$Hour), length)
par(las=2)
barplot(eventidbyhours,
        main = "Event IDs by Hours",
        xlab = "Hours",
        ylab = "Frequency",
        col = "lightblue",
        cex.names=0.8
)

## EventID qty by hours segmented by Event Type
eventidbyhours <- table(cleandf$EventTypeName, cleandf$Hour)
par(las=2)
barplot(eventidbyhours,
        main = "Hourly Event IDs by Event Type",
        xlab = "Hours",
        ylab = "Frequency",
        col = c("salmon","skyblue", "lightgreen", "yellow"),
        cex.names=0.8
)
legend("topright", 
       legend = sort(unique(cleandf$EventTypeName)),
       col = c("salmon","skyblue", "lightgreen", "yellow"),
       fill=c("salmon","skyblue", "lightgreen", "yellow"),
       cex=0.75)

## Error qty by hours
cleandf_erroronly <- subset(cleandf, EventTypeName == 'Error')
erroridbyhours <- tapply(cleandf_erroronly$EventID, list(cleandf_erroronly$Hour), length)
rm(cleandf_erroronly)
par(las=2)
barplot(erroridbyhours,
        main = "Error IDs by Hours",
        xlab = "Hours",
        ylab = "Frequency",
        col = "lightblue",
        cex.names=0.8
)

## Single Error ID by Days
selected_event <- c(7011)
cleandf_singleerroronly <- subset(cleandf, EventID %in% selected_event)
erroridbyhours <- tapply(cleandf_singleerroronly$EventID, list(cleandf_singleerroronly$Date), length)
rm(cleandf_singleerroronly)
par(las=2)
barplot(erroridbyhours,
        main = paste("Error IDs by Day\nError:", selected_event),
        xlab = "Date",
        ylab = "Frequency",
        col = "salmon",
        cex.names=0.8
)
rm(selected_event)

## Single Error ID by hours
cleandf_singleerroronly <- subset(cleandf, EventID %in% c(7011))
erroridbyhours <- tapply(cleandf_singleerroronly$EventID, list(cleandf_singleerroronly$Hour), length)
rm(cleandf_singleerroronly)
par(las=2)
barplot(erroridbyhours,
        main = "Error IDs by Hours",
        xlab = "Hours",
        ylab = "Frequency",
        col = "lightblue",
        cex.names=0.8
)



## Errors/Events in Single Day
today <- '2017-10-17'
singledayevent <- subset(cleandf, Date == today)
oneday_eventlist <- table(singledayevent$EventTypeName, singledayevent$EventSource)
oneday_eventlist <- oneday_eventlist[,colSums(oneday_eventlist)>1]
par(las=2)
par(mar=c(5.1,16.1,4.1,2.1))
bp <- barplot(oneday_eventlist,
        main = paste0("Events on ",today, "\n", filename),
        xlab = "",
        ylab = "",
        col=c("salmon","skyblue", "lightgreen", "yellow"),
        cex.names=0.6, horiz=TRUE
        )
legend("topright", 
       legend = sort(unique(singledayevent$EventTypeName)),
       #col = c("salmon","skyblue", "lightgreen", "yellow"),
       fill=c("salmon","skyblue", "lightgreen", "yellow"),
       cex=0.65,
       bty='n',
       y.intersp = 0.7,
       x.intersp = 0.7,
       ncol=2)
rm(singledayevent, today)

## Errors/Events in Single Day by hours
today <- '2017-10-17'
singledayevent <- subset(cleandf, Date == today & EventTypeName == 'Error')
oneday_hourlyeventlist <- table(singledayevent$EventID, singledayevent$Hour)
oneday_hourlyeventlist <- oneday_hourlyeventlist[rowSums(oneday_hourlyeventlist)>1,]
par(las=2)
par(mar=c(9.1,4.1,4.1,2.1))
par(xpd=TRUE)
barplot(oneday_hourlyeventlist,
        main = paste0("Events on ",today, " by Hours\n", filename),
        xlab = "Hours",
        ylab = "Frequency",
        col = rainbow(7, alpha=0.8),
        #col=c("salmon","skyblue", "lightgreen", "yellow"),
        cex.names=0.6, bty="L"
)
legend("top", 
       #legend = sort(unique(singledayevent$EventID)),
       legend = rownames(oneday_hourlyeventlist),
       #col = c("salmon","skyblue", "lightgreen", "yellow"),
       fill=rainbow(7, alpha=0.8),
       cex=0.55,
       ncol=4)
rm(singledayevent)




#################################################################


