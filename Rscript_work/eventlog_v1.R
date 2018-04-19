#### Script to read Event Log File ####

#### Initialize ####
setwd("C:/NotBackedUp")
getwd()
#.libPaths('C:/NotBackedUp/R/win-library/3.4.1')
rm(list=ls())
####################

#### Library ####
require(stringr)
require(ggplot2)
#################

## Invoke log parser function
##

################## Read and Clean Data ####################

## Read the resulting csv file
rm(list=ls())
app <- "datafiles/eventlogdata/MyApplication.csv"
system <- "datafiles/eventlogdata/MySystem.csv"
app_df <- read.csv(app, as.is=TRUE, nrows=-1)
sys_df <- read.csv(system, as.is=TRUE, nrows=-1)


## Clean data

## Tag Application and System Log
app_df$LogType <- 'Application'
sys_df$LogType <- 'System'
app_df$Filename <- sub('.*/','\\1',app)
sys_df$Filename <- sub('.*/','\\1',system)
rm(app, system)

  ## Combine both App and System DataFrame
df <- rbind(app_df, sys_df)
rm(app_df, sys_df)

# Remove unneeded columns: EventLog, TimeGenerated, Data, SID
drops <- c("EventLog","TimeGenerated", "Data", "SID")
df <- df[ , !(names(df) %in% drops)]
rm(drops)

# Parse date and time
dfdatetime <- strsplit(df$TimeWritten, split=" ")
df$Date <- sapply(dfdatetime,'[[',1)
df$Time <- sapply(dfdatetime,'[[',2)
rm(dfdatetime)

# Add hour column
df$Hour <- paste0(substr(df$Time, 0, 2),":00:00")

# Format Date & Time into Date and POSIXct class
df$Date <- as.Date(df$Date)
df$Time <- as.POSIXct(df$Time, format='%H:%M:%S')

# Order based on Date & Time
df <- df[order(df$Date, df$Time),]

# Combine Event + SourceName
df$EventSource <- paste(df$EventID, '-', df$SourceName)

# Split DF into app & system df again for more focused analysis
app_df <- subset(df, LogType == 'Application')
sys_df <- subset(df, LogType == 'System')

#############################################################


###################### SUBSET Data  #########################

## Subset for the last 92 days
df$Date <- as.Date(df$Date)
df <- subset(df, Date > max(Date)-92)
df$Date <- as.character(df$Date)

## Exclude certain EventID
excluded_eventID <- c(7023, 7024, 10010, 1)
df <- subset(df, !(EventID %in% excluded_eventID))
rm(excluded_eventID)

## Select ONLY certain eventID/s
included_eventID <- 6006
df <- subset(df, EventID %in% included_eventID)
rm(included_eventID)


#############################################################



#################### TOP Error per Logs ########################

## TOP Error for Application
app_error <- subset(df, LogType == 'Application' & EventTypeName == 'Error event')
top.app_error <- sort(table(app_error$EventSource),decreasing = TRUE)
if(length(top.app_error) > 10)
{
  top.app_error <- top.app_error[1:10]
}
top.app_error <- as.data.frame(top.app_error)
names(top.app_error) <- c('EventSource', 'Frequency')
top.app_error$EventSource <- as.character(top.app_error$EventSource)
eventsource <- strsplit(top.app_error$EventSource, split=" - ")
top.app_error$EventID <- sapply(eventsource,'[[',1)
top.app_error$SourceName <- sapply(eventsource,'[[',2)
rm(eventsource)
top.app_error$Status <- 'Error'
top.app_error$LogType <- 'Application'

## TOP Error for System
sys_error <- subset(df, LogType == 'System' & EventTypeName == 'Error event')
top.sys_error <- sort(table(sys_error$EventSource),decreasing = TRUE)
if(length(top.sys_error) > 10)
{
  top.sys_error <- top.sys_error[1:10]
}
top.sys_error <- as.data.frame(top.sys_error)
names(top.sys_error) <- c('EventSource', 'Frequency')
top.sys_error$EventSource <- as.character(top.sys_error$EventSource)
eventsource <- strsplit(top.sys_error$EventSource, split=" - ")
top.sys_error$EventID <- sapply(eventsource,'[[',1)
top.sys_error$SourceName <- sapply(eventsource,'[[',2)
rm(eventsource)
top.sys_error$Status <- 'Error'
top.sys_error$LogType <- 'System'



################################################################

###################### Data Visualization ######################

## BARPLOT for top errors
## Need eventID, no of errors, eventID description, status
errors <- top.app_error

g <- ggplot(data=errors, aes(x=reorder(EventID,-Frequency), y=Frequency, fill=EventSource)) +
  geom_bar(stat='identity' ,position='stack') +
  ggtitle(paste('Top', unique(errors$Status), errors$LogType ,'Log', 
                unique(df$ComputerName),
                '\nPeriod:', min(df$Date), 'to', max(df$Date))) +
  scale_fill_discrete(name='EventID', 
                      breaks=errors$EventSource, 
                      labels=paste(errors$EventID,'-',  errors$SourceName)
  ) + 
  geom_text(position='stack',aes(label=Frequency), vjust = -0.5, size=3) +
  xlab("EventID")
g

rm(app_error, sys_error)
#################################################################



################ Reading Errors per hour v2 ####################

#### APPLICATION Error
app_error <- subset(df, LogType == 'Application' & EventTypeName == 'Error event')
top.app_error <- sort(table(app_error$EventSource),decreasing = TRUE)
if(length(top.app_error) > 10)
{
  top.app_error <- top.app_error[1:10]
}
df.top_app_error <- subset(df, EventSource %in% names(top.app_error))

app_error_hourly <- aggregate(df.top_app_error$EventSource, 
                             by=list(df.top_app_error$EventSource, df.top_app_error$Hour), 
                             length)
names(app_error_hourly) <- c('EventSource', 'Hour', 'Frequency')
app_error_hourly$EventSource <- as.character(app_error_hourly$EventSource)

eventsource <- strsplit(app_error_hourly$EventSource, split=" - ")
app_error_hourly$EventID <- sapply(eventsource,'[[',1)
app_error_hourly$SourceName <- sapply(eventsource,'[[',2)
rm(eventsource)
app_error_hourly$LogType <- 'Application'
app_error_hourly$Status <- 'Error'




#### SYSTEM Error
sys_error <- subset(df, LogType == 'System' & EventTypeName == 'Error event')
top.sys_error <- sort(table(sys_error$EventSource),decreasing = TRUE)
if(length(top.sys_error) > 10)
{
  top.sys_error <- top.sys_error[1:10]
}
df.top_sys_error <- subset(df, EventSource %in% names(top.sys_error))



sys_error_hourly <- aggregate(df.top_sys_error$EventSource, 
                              by=list(df.top_sys_error$EventSource, df.top_sys_error$Hour), 
                              length)
names(sys_error_hourly) <- c('EventSource', 'Hour', 'Frequency')
sys_error_hourly$EventSource <- as.character(sys_error_hourly$EventSource)

eventsource <- strsplit(sys_error_hourly$EventSource, split=" - ")
sys_error_hourly$EventID <- sapply(eventsource,'[[',1)
sys_error_hourly$SourceName <- sapply(eventsource,'[[',2)
rm(eventsource)
sys_error_hourly$LogType <- 'System'
sys_error_hourly$Status <- 'Error'

#### Data Visualization

## Stacked Barplot Application
g <- ggplot(data=app_error_hourly, aes(x=Hour, y=Frequency, fill=EventSource)) +
  geom_bar(stat='identity', color='white') +
  scale_fill_discrete(name='EventID', 
                      breaks=app_error_hourly$EventSource, 
                      labels=paste(app_error_hourly$EventID,'-', app_error_hourly$SourceName)
  ) + 
  geom_text(size = 3, position = position_stack(vjust = 0.5), aes(label=Frequency)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  ggtitle(paste('Error per Hour', app_error_hourly$LogType ,'Log', '\n',
                unique(df$ComputerName), 'Period', min(df$Date), max(df$Date)) ) + 
  xlab('EventID')
g


## Stacked Barplot System
g <- ggplot(data=sys_error_hourly, aes(x=Hour, y=Frequency, fill=EventSource)) +
  geom_bar(stat='identity', color='white') +
  scale_fill_discrete(name='EventID', 
                      breaks=sys_error_hourly$EventSource, 
                      labels=paste(sys_error_hourly$EventID,'-', sys_error_hourly$SourceName)
  ) + 
  geom_text(size = 3, position = position_stack(vjust = 0.5), aes(label=Frequency)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  ggtitle(paste('Error per Hour', sys_error_hourly$LogType ,'Log', '\n',
                unique(df$ComputerName), 'Period', min(df$Date), '-', max(df$Date)) ) + 
  xlab('EventID')
g


##Write into file
cols_to_extract <- c("EventID", "SourceName", "EventTypeName", "EventCategoryName", "Message")
write.csv(unique(df.top_sys_error[,cols_to_extract]), "Tara_sys_log.csv")
rm(cols_to_extract)

rm(df.top_app_error, app_error, top.app_error)
rm(df.top_sys_error, sys_error, top.sys_error)

#############################################################


###################### Correlating Data ######################

## Check Top errors
app_error <- subset(df, LogType == 'Application' & EventTypeName == 'Error event')
top.app_error <- sort(table(app_error$EventSource),decreasing = TRUE)
if(length(top.app_error) > 10)
{
  top.app_error <- top.app_error[1:10]
}
df.top_app_error <- subset(df, EventSource %in% names(top.app_error))

## Choose between Application DF only, System DF Only, or App+Sys DF
tgt_df <- app_df

temp_df <- data.frame(prev5=c(),
                      prev4=c(),
                      prev3=c(),
                      prev2=c(),
                      prev1=c(),
                      main_error=c())

corr_df <- tgt_df[0,c('RecordNumber','ComputerName', 'Filename',
                                  'Date','Time','TimeWritten', 'LogType','EventID',
                                  'SourceName','EventSource','EventTypeName', 
                                  'EventCategoryName'
)]

for(i in 1:length(top.app_error))
{
  ## Find record number which corresponds to top error
  toperrorID <- names(top.app_error)[i]
  idloc <- which(tgt_df$EventSource == toperrorID)
  
  ## Checking the id for sequence of events before main error
  seq_id <- c()
  for(j in 1:length(idloc))
  {
    #print(idloc[i])
    #x <- append(x, (idloc[i]-10):(idloc[i]+5) )
    seq_id <- append(seq_id, (idloc[j]-5):(idloc[j]) )
  }
  eventseq <- rep(seq(-5,0,1),length(idloc))
  
  ## Creating dataframe of the detailed event sequences
  correlation.df <- tgt_df[seq_id,c('RecordNumber','ComputerName', 'Filename',
                                    'Date','Time','TimeWritten', 'LogType','EventID',
                                    'SourceName','EventSource','EventTypeName', 
                                    'EventCategoryName'
                                    )]
  correlation.df$EventSequence <- eventseq
  correlation.df$Main_error <- toperrorID
  correlation.df$Tag <- NA
  correlation.df$Occurence <- rep(1:length(idloc),each=6)
  correlation.df[correlation.df$EventSequence == 0,]$Tag <- 'focal_error'
  
  corr_df <- rbind(corr_df, correlation.df)
  
  ## Creating dataframe of error sequence
  sequence_df <- data.frame(prev5=correlation.df[correlation.df$EventSequence == -5,]$EventID,
                            prev4=correlation.df[correlation.df$EventSequence == -4,]$EventID,
                            prev3=correlation.df[correlation.df$EventSequence == -3,]$EventID,
                            prev2=correlation.df[correlation.df$EventSequence == -2,]$EventID,
                            prev1=correlation.df[correlation.df$EventSequence == -1,]$EventID,
                            main_error=correlation.df[correlation.df$EventSequence == 0,]$EventID,
                            eventsource=names(top.app_error[i]),
                            DateTime=correlation.df[correlation.df$EventSequence == 0,]$TimeWritten)
  
  ## Aggregating all dataframe into temp_df
  temp_df <- rbind(temp_df, sequence_df)
  if(i == 1) { print("temp_df size:") }
  
  print(dim(temp_df))
}

## Removing unused variables
rm(i,j,toperrorID,seq_id,sequence_df,eventseq, correlation.df,
   top.app_error,df.top_app_error, app_error, idloc)


## Write into file 
write.csv(temp_df,'temp_df.csv')
write.csv(corr_df,'corr_df.csv')

## Visualize
ggplot(data=correlation.df[1:100,], aes(x=EventSequence, y=Occurence, color=factor(EventID) ))+
  geom_point(size=3, alpha=0.6) +
  geom_text(aes(label=EventID),size=3, hjust=-0.2, vjust=-0.5)

## Visualize Sankey
library(dplyr)
library(reshape2)

temp_df <- read.csv('temp_df.csv')
temp_df <- temp_df[,-1]
head(temp_df)
temp_df$eventsource <- NULL
temp_df$DateTime <- NULL
temp_df <- subset(temp_df, main_error %in% c(13100, 1001, 3, 8194)   )

temp_plot <- data.frame()

for(i in 2:ncol(temp_df))
{
  temp_cache <- temp_df %>% 
    group_by(temp_df[,i-1],temp_df[,i]) %>% 
    summarise(n=n()) %>% 
    ungroup()
  
  colnames(temp_cache)[1:2] <- c('from', 'to')
  temp_cache$from <- paste(temp_cache$from, '(', i-1, ')', sep='')
  temp_cache$to <- paste(temp_cache$to, '(', i, ')', sep='')
  
  temp_plot <- rbind(temp_plot, temp_cache)
  
}

plot(gvisSankey(temp_plot, from='from', to='to', weight='n',
                options=list(height=900, width=1800, sankey="{link:{color:{fill:'lightblue'}}}")))


################################################################


############## Statistics for Data Correlation #################


################################################################
















#################### PROCESSING DATA ########################
# check the top error
topeventIDerrors <- sort(table(df.error$EventID), decreasing = TRUE)[1:10]

# subset the top error
toperror <- subset(df.error, EventID %in% as.integer(names(topeventIDerrors)) )


toperror.df <- as.data.frame(topeventIDerrors)
names(toperror.df) <- c("EventID", "Frequency")


# subset apps.error for only top error
apps.error.top <- subset(apps.error, EventID %in% toperror.df$EventID)

# select only certain columns
apps.error.top2 <- subset(apps.error.top, select = c(EventID, EventTypeName, 
                                                     EventCategoryName,SourceName))


#grep('Group Policy', apps.error.top2$SourceName, value=T)
apps.error.top2$SourceName <- gsub('Group Policy.+', 'Group Policy', apps.error.top2$SourceName)
  
#take unique col
apps.error.top2 <- (unique(apps.error.top2))

toperror.df2 <- merge(toperror.df, apps.error.top2, by="EventID")
toperror.df2 <- subset(toperror.df2, select=c(EventID, Frequency, SourceName))
toperror.df2 <- unique(toperror.df2)
toperror.df2 <- toperror.df2[order(-toperror.df2$Frequency),]


## TOP error
g <- ggplot(data=toperror.df, aes(x=EventID, y=Frequency, fill=EventID)) +
  geom_bar(stat='identity') +
  ggtitle('Top Error System Log') +
  scale_fill_discrete(name='EventID', 
                      breaks=toperror.df2$EventID, 
                      labels=paste(toperror.df2$EventID,'-',  toperror.df2$SourceName)
  ) + 
  geom_text(aes(label=Frequency), vjust = -0.5)
g

#############################################################



################ Reading Errors per hour ####################
errorperhour.df <- aggregate(toperror$EventID, 
                             by=list(toperror$EventID, toperror$Hour), 
                             length)
names(errorperhour.df) <- c('EventID', 'Hour', 'Frequency')
errorperhour.df$EventID <- as.character(errorperhour.df$EventID)

eventsource <- unique(apps.error[,c('EventID', 'SourceName')])
eventsource$SourceName <- gsub('Group Policy.+', 'Group Policy', eventsource$SourceName)
eventsource <- unique(eventsource)
errorperhour.df2 <- merge(errorperhour.df, eventsource, by="EventID")
errorperhour.df2 <- errorperhour.df2[order(errorperhour.df2$Hour),]

## Stacked Barplot
g <- ggplot(data=errorperhour.df2, aes(x=Hour, y=Frequency, fill=EventID, order=Frequency), order=Frequency) +
  geom_bar(stat='identity', color='white') +
  ggtitle('Error per Hour System Log') +
  scale_fill_discrete(name='EventID', 
                      breaks=errorperhour.df2$EventID, 
                      labels=paste(errorperhour.df2$EventID,'-', errorperhour.df2$SourceName)
  ) + 
  geom_text(size = 3, position = position_stack(vjust = 0.5), aes(label=Frequency))
g

##Write into file
cols_to_extract <- c("EventID", "SourceName", "EventTypeName", "EventCategoryName", "Message")
write.csv(unique(toperror[,cols_to_extract]), "systemlog.csv")
rm(cols_to_extract)

#############################################################




################## Comparison 1 Week ####################

##scratchpad
#subset(df, grepl('warning', EventTypeName, ignore.case = TRUE) & Date == '2017-08-22')[,c('EventID', 'SourceName','Date','Time')]

# table of most common event id
table(subset(df, grepl('information', EventTypeName, ignore.case = TRUE) & Date == '2017-08-23')$EventID)

# table of event IDs
sort(table(subset(df, Date == '2017-08-22')$EventID),decreasing = T)
sort(table(subset(df, Date == '2017-08-23')$EventID),decreasing = T)
eventidinfo <- unique(df[,c('EventID', 'Message')])


# Show 1 week of number of events
table(df$Date)

oneweekdata <- aggregate(df$EventType, by=list(df$EventType, df$Date), length)
names(oneweekdata) <- c('EventType', 'Date', 'Frequency')
eventtype <- unique(df[,c('EventType','EventTypeName')])
oneweekdata <- merge(oneweekdata, eventtype, by="EventType")
oneweekdata[order(oneweekdata$Date, oneweekdata$EventType),]
oneweekdata$EventType <- as.factor(oneweekdata$EventType)

g <- ggplot(data=oneweekdata, aes(x=Date, y=Frequency, fill=EventType)) +
     geom_bar(stat='identity', color='white') +
     ggtitle('1 Week Running Data') +
     # scale_fill_discrete(name='EventType', 
     #                     breaks=oneweekdata$EventType, 
     #                     labels=paste(oneweekdata$EventType,'-', oneweekdata$EventTypeName)
     #                    ) + 
     scale_fill_manual(name = 'Event Type',
                    values=c('salmon','gold','lightgrey'), 
                    breaks=oneweekdata$EventType,
                    labels=paste(oneweekdata$EventType, oneweekdata$EventTypeName, "111")) +
     geom_text(size = 3, position = position_stack(vjust = 0.5), aes(label=Frequency))
g

df.errorwarning <- subset(df, grepl('warning|error',df$EventTypeName, ignore.case = T) & Date > '2017-08-21')

cols_to_extract <- c("EventID", "SourceName", "EventTypeName", "EventCategoryName", "Message")
write.csv(unique(df.errorwarning[,cols_to_extract]), "systemlog.csv")
rm(cols_to_extract, df.errorwarning)
############################################################



######### Comparing Yesterday to Today Error and Warnings ############

df.2day <- subset(df, Date > Sys.Date()-2)
alert_error <- 'error'
alert_warning <- 'warning'
df.alert2day <- subset(df.2day, grepl(alert_error,df.2day$EventTypeName, ignore.case = T))
rm(alert_error, alert_warning)

######## Process Data
topalert.prevday <- sort(table(df.alert2day[df.error2day$Date == Sys.Date()-1,]$EventSource), decreasing = T)
topalert.today <- sort(table(df.alert2day[df.error2day$Date == Sys.Date(),]$EventSource), decreasing = T)

## PrevDay
topalert.prevday <- as.data.frame(topalert.prevday)
names(topalert.prevday) <- c('EventSource', 'Frequency')
topalert.prevday$Date <- min(df.alert2day$Date)

## Today
topalert.today <- as.data.frame(topalert.today)
names(topalert.today) <- c('EventSource', 'Frequency')
toperror.today$Date <- max(df.alert2day$Date)


## Start plot
topalert.df <- topalert.today
eventid <- sapply(strsplit(as.character(topalert.df$EventSource), split='-'),'[[',1)
topalert.df$EventID <- eventid
rm(eventid)

g <- ggplot(data=topalert.df, aes(x=reorder(EventID, -Frequency), 
                                           y=Frequency, fill=EventSource)) +
  geom_bar(stat='identity', position = 'dodge') +
  ggtitle(paste('Top Warnings System Log',min(topalert.df$Date)) )+
  # scale_fill_discrete(name='EventSource', 
  #                     breaks=toperror.df$EventSource, 
  #                     labels=paste(toperror.df$EventSource,'-',  toperror.df$SourceName)
  # ) + 
  geom_text(aes(label=Frequency), vjust = -0.5) +
  xlab('EventID') + ylab('Frequency')
g

########################################################################



################### Find The trend of certain EventID ##################
## SELECT Only certain COLUMN
subsetcol <- c('EventID', 'Strings','TimeWritten', 'Date', 'Time')
df <- df[,subsetcol]
rm(subsetcol)

## Split String to get the seconds
durationlist <- strsplit(df$Strings, split='|', fixed=TRUE)
duration <- as.integer(sapply(durationlist,'[[',2))
df$Duration <- duration
rm(durationlist, duration)

g <- ggplot(data=df, aes(x=Date, y = Duration)) +
     geom_point(size=3 ,color='salmon' ,alpha=0.5) +
     geom_text(size=3, aes(label=Duration), hjust=-0.8) +
     ggtitle('EventID 6006 Time to log duration')
g

df22 <- subset(df2, !(EventID %in% c(7036,5002)))
df22 <- subset(df2, grepl('error', EventTypeName, ignore.case = TRUE))
df22$EventID <- as.factor(df22$EventID)
toperror <- sort(table(df22$EventID), decreasing = T)[1:20]
df22 <- subset(df22, EventID %in% names(toperror))
df22$EventID <- as.factor(df22$EventID)
levels(df22$EventID) <- names(toperror)

df22 <- subset(df22, !(EventID %in% c(36888,36874)))

ggplot(df22, aes(Date,Time,color=EventID)) + 
  geom_point(size=3, alpha=0.7) + 
  scale_y_datetime(date_labels="%H:%M") +
  geom_text(size=3, hjust=-0.2,aes(label=df22$EventID))
########################################################################
