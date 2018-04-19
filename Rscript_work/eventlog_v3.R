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
errordf <- subset(cleandf, grepl('ACCA.exe', cleandf$Description) & EventTypeName == 'Error' & EventID == 1000)



####################### Analysis ########################

require(ggplot2)
# Occurence by Date
datetrend_df <- as.data.frame(table(cleandf$Date))
names(datetrend_df) <- c("Date", "Freq")
dateplot <- ggplot(data=datetrend_df, aes(x=Date, y=Freq)) + 
            geom_line(aes(group=1), colour='red') + 
            geom_point(colour='red')
dateplot


# Occurence by Hour
hourtrend_df <- as.data.frame(table(cleandf$Hour))
names(hourtrend_df) <- c("Hour", "Freq")
hourplot <- ggplot(data=hourtrend_df, aes(x=Hour, y=Freq)) + 
            geom_line(aes(group=1), colour='skyblue') + 
            geom_point(colour='skyblue')
hourplot


# Unique value for each column
as.data.frame(apply(cleandf, 2, function(x) length(unique(x))))


# Sequencing errors
indexloc <- which(cleandf$EventTypeName == 'Error' & grepl('ACCA.exe', cleandf$Description) & cleandf$EventID == 1000)
prev_n <- 5
indexlocprev <- indexloc - prev_n
indexlocprev <- ifelse(indexlocprev < 1, 1, indexlocprev)
listindex <- mapply(':', indexlocprev, indexloc)
listindex <- as.vector(listindex)
#Alternative
# listindex <- unlist(Map(seq, indexlocprev, indexloc))

maindetailseq_df <-  cleandf[listindex,]

eventseq <- seq(-prev_n, 0)
EventSequence <- rep(eventseq, length(indexloc))
Nth_Occurence <- rep(1:length(indexloc), each=length(eventseq))

maindetailseq_df$EventSequence <- EventSequence
maindetailseq_df$Nth_Occurence <- Nth_Occurence

# Creating Wide format sequence
require(reshape)
#cast(maindetailseq_df, rowindex ~ column)
seq_df <- cast(maindetailseq_df, Nth_Occurence ~ EventSequence, value = 'EventID')
seq_df <- data.frame(lapply(seq_df, as.character), stringsAsFactors=FALSE)
event_source <- unique(maindetailseq_df[maindetailseq_df$EventSequence==0,]$EventSource)
seq_df$EventSource <- event_source
event_datetime <- maindetailseq_df[maindetailseq_df$EventSequence==0,]$DateTime
seq_df$DateTime <- event_datetime

seq_df2 <- cast(maindetailseq_df, Nth_Occurence ~ EventSequence, value = 'EventSource')
seq_df2 <- data.frame(lapply(seq_df2, as.character), stringsAsFactors=FALSE)
names(seq_df2) <- c('Nth_Occurence', '-5', '-4', '-3', '-2', '-1', '0')

seq_df$EventSource <- NULL
seq_df$DateTime <- NULL

require(dplyr)
temp_plot <- data.frame()

for(i in 2:ncol(seq_df2))
{
  temp_cache <- seq_df2 %>% 
    group_by(seq_df2[,i-1],seq_df2[,i]) %>% 
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
temp_plot2 <- subset(temp_plot, n > 3)
#from_int <- temp_plot2$from_int
#to_int <- temp_plot2$to_int
temp_plot2$from_int <- NULL
temp_plot2$to_int <- NULL
from <- temp_plot2$from
to <- temp_plot2$to

## Creating color-coding for different type of events
nodestringlist <- paste(from,to, collapse=' ')
nodestringvector <- strsplit(nodestringlist, split =' ')
nodestringvector <- nodestringlist
node_order <- unique(nodestringvector[[1]])
node_id_list <- strsplit(node_order,split="(",fixed=T)
node_id <- sapply(node_id_list,'[[',1)
node_df <- data.frame(NodeString = node_order, 
                      EventID=node_id
)
detach("package:dplyr", unload=TRUE)
library(plyr)
node_df2 <- join(node_df, 
                 eventsource[,c('EventID','EventTypeName', 'EventColor')], 
                 by='EventID')
node_df2 <- unique(node_df2)
detach("package:plyr", unload=TRUE)
print(node_df2)

sankey_node_col <- paste(paste0("'",node_df2$EventColor,"'"),collapse = ",")
sankey_node_col <- paste0("[",sankey_node_col,"]")
(sankey_node_col)

sankey_node_opts <- paste0("node:", "{","colors:", sankey_node_col,"}")
sankey_node_opts
sankey_link_opts <- paste0("link:","{", "colorMode: 'gradient'" ,"}")
sankey_link_opts
sankey_opt_string <- paste("{", sankey_link_opts, ",", sankey_node_opts  ,"}")
sankey_opt_string

rm(sankey_node_col, sankey_link_opts, sankey_node_opts,
   from, to, i,
   temp_cache, node_df, node_df2)

require(googleVis)
servername = strsplit(filename, ".", fixed=T)[[1]][1]
servername = 'Hamish'
sankeyoption = list(height=900, width = 1800,
                    sankey=sankey_opt_string)
sankeychart = gvisSankey(data=temp_plot2, from='from', to='to', weight='n', 
                         chartid = servername,
                         options=sankeyoption)
chart_title <- paste("<h1 style='text-align:center;'>","Sankey", servername, "</h1>")
sankeychart$html$chart[1] <- paste(chart_title, sankeychart$html$chart[1])
## Remove any footer about GoogleVis
sankeychart$html$footer <- NULL
sankeychart$html$caption <- NULL
plot(sankeychart)

