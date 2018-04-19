#### Script to read Event Log File from csv/xls ####

######## Initialize ########
setwd("C:/NotBackedUp")
getwd()
.libPaths('C:/NotBackedUp/R/win-library/3.4.1')
rm(list=ls())
options(browser = "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe")
############################

######## Library ########
require(ggplot2)
require(googleVis)

#########################

################## Read and Clean Data ####################

## Read the resulting csv file
folderloc <- "datafiles/eventlogdata/"
filename <- "HYPAU201MEL0093.globaltest.anz.com-Sep2017_sample.csv"
fileloc <- paste0(folderloc,filename)
df <- read.csv(fileloc, as.is=TRUE, nrows=-1)

## Clean header name
names(df) <- c('EventID', 'Type', 'ServerName', 'Description',
               'SourceName','DateTime','EventTypeName')

## Clean Data Type
# eventid to factor
#df$EventID <- as.factor(df$EventID)

## Parsing Date and Time
dfdatetime <- strsplit(df$DateTime, split="T")
df$Date <- sapply(dfdatetime,'[[',1)
df$Time <- sapply(dfdatetime,'[[',2)
rm(dfdatetime)

df$Date <- as.Date(df$Date)

# Order based on Date & Time
df <- df[order(df$Date, df$Time),]

# Add hour column
df$Hour <- paste0(substr(df$Time, 0, 2),":00:00")

## Join EventID + ServiceType
df$EventSource <- paste(df$EventID, '-', df$SourceName)


## Exclude EventID: 7036
excluded_eventID <- c(7036)
df <- subset(df, !(EventID %in% excluded_eventID))
rm(excluded_eventID)


## Write cleaned data into CSV File
write.csv(df, paste0(folderloc,'Cleaned_',filename))
rm(fileloc, folderloc)

###########################################################


################## Data Reference ####################

eventsource <- unique(df[,c('EventID','EventSource', 'EventTypeName')])
eventsource <- eventsource[order(eventsource$EventID),]
eventtypecolor <- data.frame(EventTypeName = c('Information', 'Warning', 'Error', 'Critical'),
                             EventColor = c('blue', 'yellow', 'salmon', 'red')
)
eventsource <- merge(eventsource,eventtypecolor, by="EventTypeName", sort=FALSE)


eventsource_withDesc <- merge(eventsource,df[c('EventID','Description')],by="EventID", sort=FALSE)
eventsource_withDesc <- unique(eventsource_withDesc)
write.csv(eventsource_withDesc, paste0('EventIDRef_',filename))

rm(eventtypecolor, eventsource_withDesc)

######################################################


################## Subset Data ####################

## Subset Error and Warning data only
clean_df <- subset(df, Status %in% c('Warning', 'Error'))


## Subset data from last 3 months
clean_df <- subset(clean_df, Date > max(Date)-92)


## Divide data into Application & System
app_df <- subset(clean_df, Type == 'Application')
sys_df <- subset(clean_df, Type == 'System')

#######################################################




################## Data Processing ####################

## TOP 10 error
errors <- sort(table(app_df[app_df$Status == 'Error',]$EventService),decreasing = T)
if(length(errors) > 10)
{
  errors <- errors[1:10]
}

errors <- as.data.frame(errors)
names(errors) <- c('EventService', 'Frequency')
errors$EventService <- as.character(errors$EventService)
eventservice <- strsplit(errors$EventService, split=" - ")
errors$EventID <- sapply(eventservice,'[[',1)
errors$ServiceType <- sapply(eventservice,'[[',2)
rm(eventservice)
errors$Status <- 'Error'

#### ******************* #####
errors <- sort(table(app_df[app_df$Status == 'Error',]$EventID),decreasing = T)
if(length(errors) > 10)
{
  errors <- errors[1:10]
}
errors <- as.data.frame(errors)
names(errors) <- c('EventID', 'Frequency')

ggplot(data=errors, aes(x=EventID, y=Frequency, fill=EventID))+geom_bar(stat='identity')

ggplot(data=x, aes(x=EventID, y=Frequency, fill=EventID))+geom_bar(stat='identity')
#### ******************* #####


## TOP 10 warning
warnings <- sort(table(app_df[app_df$Status == 'Warning',]$EventService),decreasing = T)
if(length(warnings) > 10)
{
  warnings <- warnings[1:10]
}

warnings <- as.data.frame(warnings)
names(warnings) <- c('EventService', 'Frequency')
warnings$EventService <- as.character(warnings$EventService)
eventservice <- strsplit(warnings$EventService, split=" - ")
warnings$EventID <- sapply(eventservice,'[[',1)
warnings$ServiceType <- sapply(eventservice,'[[',2)
rm(eventservice)
warnings$Status <- 'Warning'


#######################################################



################## Data Visualization ####################

## BARPLOT for top errors
## Need eventID, no of errors, eventID description, status
g <- ggplot(data=errors, aes(x=reorder(EventID,-Frequency), y=Frequency, fill=EventService)) +
  geom_bar(stat='identity') +
  ggtitle(paste('Top Error', unique(errors$Status), 'App Log', 
                unique(clean_df$ServerName),
                '\nPeriod:', min(clean_df$Date), 'to', max(clean_df$Date))) +
  scale_fill_discrete(name='EventID', 
                      breaks=errors$EventService, 
                      labels=paste(errors$EventID,'-',  errors$ServiceType)
  ) + 
  geom_text(aes(label=Frequency), vjust = -0.5)
g

## BARPLOT for top warnings
## Need eventID, no of warnings, eventID description, status
g <- ggplot(data=warnings, aes(x=reorder(EventID,-Frequency), y=Frequency, fill=EventService)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle(paste('Top', unique(warnings$Status), 'App Log', 
                unique(clean_df$ServerName),
                '\nPeriod:', min(clean_df$Date), 'to', max(clean_df$Date))) +
  scale_fill_discrete(name='EventID', 
                      breaks=warnings$EventService, 
                      labels=paste(warnings$EventID,'-',  warnings$ServiceType)
  ) + 
  geom_text(aes(label=Frequency), vjust = -0.5)
g


##########################################################


##################### Scratchpad #########################
app_df <- subset(df, Type == 'Application')
sys_df <- subset(df, Type == 'System')


######## APPLICATION ########
## Check top error from Application
top_app_error <- sort(table(app_df[app_df$Status=='Error',]$EventID),decreasing = T)
top_app_error <- as.data.frame(top_app_error)
names(top_app_error) <- c('EventID', 'Frequency')

## Check top 30 errors before the particular error


## Find ID location of a particular error
toperrorID <- as.numeric(as.character(top_app_error$EventID[1]))

idloc <- which(app_df$EventID == toperrorID)
idloc <- which(df$EventID == toperrorID)

x <- c()
for(i in 1:length(idloc))
{
  #print(idloc[i])
  x <- append(x,(idloc[i]-10):idloc[i])
}

series_df <- app_df[x,]
app_df$Hour <- paste0(substr(app_df$Time, 0, 2),":00:00")
app_df$TenMin <- paste0(substr(app_df$Time, 0, 4),"0:00")

error <- df[x,]
ggplot(data=error, aes(x=Date, y=Time, color=Status)) + 
  geom_point(size=3, alpha=0.5) + 
  geom_text(size=3, aes(label=error$EventID), vjust=0.3, hjust=-0.1) +
  ggtitle('Time Series of error events for SGLT-C4514060')

#  scale_y_time(name='Tenmin', breaks=levels(app_df$TenMin))
#############################


######## SYSTEM ########

## Check top error from System
top_sys_error <- sort(table(sys_df[sys_df$Status=='Error',]$EventID),decreasing = T)
top_sys_error <- as.data.frame(top_sys_error)
names(top_sys_error) <- c('EventID', 'Frequency')

## Find ID location of a particular error
toperrorID <- as.numeric(as.character(top_sys_error$EventID[1]))

idloc <- which(df$EventID == toperrorID)
x <- c()
for(i in 1:length(idloc))
{
  #print(idloc[i])
  x <- append(x,(idloc[i]-5):idloc[i])
}
series_df <- df[x,]
#############################

##########################################################


################## Visualization #########################
ggplot(data=subdf, aes(x=Date, y=Time, color=Status)) + 
  geom_point(size=3) + 
  scale_y_time(date_breaks="1 hour")

breaks=c("00:00:00", "01:00:00", "02:00:00", "03:00:00",
         "04:00:00", "05:00:00", "06:00:00", "07:00:00",
         "08:00:00")

hourly <- seq(from=as.POSIXct("00:00:00","%H:%M:%S", tz="UTC"),
            to=as.POSIXct("12:00:00", "%H:%M:%S", tz="UTC"),
            by="hour"
            )

##########################################################



###################### Correlating Data ######################

## Check Top errors
app_error <- subset(df, EventTypeName == 'Error')
top.app_error <- sort(table(app_error$EventSource),decreasing = TRUE)
if(length(top.app_error) > 10)
{
  top.app_error <- top.app_error[1:10]
}
df.top_app_error <- subset(df, EventSource %in% names(top.app_error))

## Choose between Application DF only, System DF Only, or App+Sys DF
tgt_df <- df

temp_df <- data.frame(prev5=c(),
                      prev4=c(),
                      prev3=c(),
                      prev2=c(),
                      prev1=c(),
                      main_error=c())

corr_df <- tgt_df[0,c('ServerName',
                      'Date','Time','DateTime', 'Type','EventID',
                      'SourceName','EventSource','EventTypeName', 'Description'
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
    seq_x <- (idloc[j]-10):(idloc[j])
    seq_x <- ifelse(seq_x < 1, NA, seq_x)
    seq_id <- append(seq_id, seq_x )
  }
  eventseq <- rep(seq(-10,0,1),length(idloc))
  
  ## Creating dataframe of the detailed event sequences
  correlation.df <- tgt_df[seq_id,c('ServerName',
                        'Date','Time','DateTime', 'Type','EventID',
                        'SourceName','EventSource','EventTypeName' ,'Description'
  )]
  
  correlation.df$EventSequence <- eventseq
  correlation.df$Main_error <- toperrorID
  correlation.df$Tag <- NA
  correlation.df$Occurence <- rep(1:length(idloc),each=0--10+1)
  correlation.df[correlation.df$EventSequence == 0,]$Tag <- 'focal_error'
  correlation.df$Filename <- filename
  
  corr_df <- rbind(corr_df, correlation.df)
  
  ## Creating dataframe of error sequence
  sequence_df <- data.frame(prev10=correlation.df[correlation.df$EventSequence == -10,]$EventID,
                            prev9=correlation.df[correlation.df$EventSequence == -9,]$EventID,
                            prev8=correlation.df[correlation.df$EventSequence == -8,]$EventID,
                            prev7=correlation.df[correlation.df$EventSequence == -7,]$EventID,
                            prev6=correlation.df[correlation.df$EventSequence == -6,]$EventID,
                            prev5=correlation.df[correlation.df$EventSequence == -5,]$EventID,
                            prev4=correlation.df[correlation.df$EventSequence == -4,]$EventID,
                            prev3=correlation.df[correlation.df$EventSequence == -3,]$EventID,
                            prev2=correlation.df[correlation.df$EventSequence == -2,]$EventID,
                            prev1=correlation.df[correlation.df$EventSequence == -1,]$EventID,
                            main_error=correlation.df[correlation.df$EventSequence == 0,]$EventID,
                            eventsource=names(top.app_error[i]),
                            DateTime=correlation.df[correlation.df$EventSequence == 0,]$DateTime)
  
  ## Aggregating all dataframe into temp_df
  temp_df <- rbind(temp_df, sequence_df)
  if(i == 1) { print("temp_df size:") }
  
  print(dim(temp_df))
}

## Removing unused variables
rm(i,j,toperrorID,seq_id,sequence_df,eventseq, correlation.df,
   top.app_error,df.top_app_error, app_error, idloc)


## Write into file 
filenametowrite_temp <- paste0('temp_df_',filename)
filenametowrite_corr <- paste0('corr_df_',filename)
write.csv(temp_df, filenametowrite_temp, row.names=FALSE)
write.csv(corr_df,filenametowrite_corr)


#### Visualize ####
# ggplot(data=correlation.df[1:100,], aes(x=EventSequence, y=Occurence, color=factor(EventID) ))+
#   geom_point(size=3, alpha=0.6) +
#   geom_text(aes(label=EventID),size=3, hjust=-0.2, vjust=-0.5)

#### Visualize Sankey ####
library(dplyr)
library(reshape2)
#library(plyr)

#detach("package:plyr", unload=TRUE)

temp_df <- read.csv(filenametowrite_temp)
head(temp_df)
temp_df$eventsource <- NULL
temp_df$DateTime <- NULL
temp_df$X <- NULL
#temp_df <- subset(temp_df, main_error %in% c(20100, 21102)   )

temp_plot <- data.frame()

for(i in 2:ncol(temp_df))
{
  temp_cache <- temp_df %>% 
    group_by(temp_df[,i-1],temp_df[,i]) %>% 
    summarise(n=n()) %>% 
    ungroup()
  
  colnames(temp_cache)[1:2] <- c('from', 'to')
  temp_cache$from_int <- paste(temp_cache$from)
  temp_cache$to_int <- paste(temp_cache$to)
  temp_cache$from <- paste(temp_cache$from, '(', i-1, ')', sep='')
  temp_cache$to <- paste(temp_cache$to, '(', i, ')', sep='')
  
  temp_plot <- rbind(temp_plot, temp_cache)
}

temp_plot2 <-  temp_plot
temp_plot2 <- subset(temp_plot, n > 10)

from_int <- temp_plot2$from_int
to_int <- temp_plot2$to_int
from <- temp_plot2$from
to <- temp_plot2$to

temp_plot2$from_int <- NULL
temp_plot2$to_int <- NULL


nodestringlist <- paste(from,to, collapse=' ')
nodestringvector <- strsplit(nodestringlist, split =' ')
node_order <- unique(nodestringvector[[1]])
node_id_list <- strsplit(node_order,split="(",fixed=T)
node_id <- sapply(node_id_list,'[[',1)
node_df <- data.frame(NodeString = node_order, 
                      EventID=node_id
                      )

## A lot of unloading/loading done here because using
## 'join' function from plyr package
detach("package:dplyr", unload=TRUE)
library(plyr)
node_df2 <- join(node_df, 
                  eventsource[,c('EventID','EventTypeName', 'EventColor')], 
                  by='EventID')
node_df2 <- unique(node_df2)
detach("package:plyr", unload=TRUE)
library(dplyr)

rm(nodestringlist,nodestringvector,node_id, node_id_list)

## Sankey Option String
sankey_node_col <- paste(paste0("'",node_df2$EventColor,"'"),collapse = ",")
sankey_node_col <- paste0("[",sankey_node_col,"]")
sankey_node_col

sankey_node_opts <- paste0("node:", "{","colors:", sankey_node_col,"}")
sankey_node_opts
sankey_link_opts <- paste0("link:","{", "colorMode: 'gradient'" ,"}")
sankey_link_opts
sankey_opt_string <- paste("{", sankey_link_opts, ",", sankey_node_opts  ,"}")
sankey_opt_string

rm(sankey_node_col, sankey_link_opts, sankey_node_opts,
   from, to, from_int, to_int, seq_x,
   filenametowrite_corr, filenametowrite_temp,i,
   temp_cache, node_df, node_df2)


## Sankey Diagram
servername = strsplit(filename, ".", fixed=T)[[1]][1]
sankeyoption = list(height=900, width = 1800,
                    sankey=sankey_opt_string)
sankeychart = gvisSankey(data=temp_plot2, from='from', to='to', weight='n', 
                         chartid = servername,
                         options=sankeyoption)

## Defining chart title as html tag
chart_title <- paste("<h1 style='text-align:center;'>","Sankey", servername, "</h1>")
sankeychart$html$chart[1] <- paste(chart_title, sankeychart$html$chart[1])

## Remove any footer about GoogleVis
sankeychart$html$footer <- NULL
sankeychart$html$caption <- NULL
plot(sankeychart)



################################################################
