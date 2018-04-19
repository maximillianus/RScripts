#### EVENT LOG BUILDING APRIOR ####

eventlog_apriori <- function(df)
{
  require(arules)
  print("#### Building Apriori... ####")
  
  ## Clean df from consecutive error rows
  df <- subset(df, `-1` != `0`)
  
  
  ## Clean df from EventName and DateTime
  df$EventSource <- NULL
  df$DateTime <- NULL
  
  
  ## number of previous events
  n_prev_event = 5
  if( ncol(df) > n_prev_event+1 )
  {
    df <- df[,(ncol(df)-n_prev_event):ncol(df)]
  }

  
  
  ## identifying the most common EVENTID
  ## In Apriori, this is called support
  frequencyrank <- as.data.frame(table(unlist(df[,1:ncol(df)-1])))[1:10,]
  names(frequencyrank) <- c('EventID', 'Frequency')
  print("Most common preceding eventID (Apriori's support):")
  print(frequencyrank[order(-frequencyrank$Frequency),])
  
  
  
  ## identifying most common SEQUENCE
  
  
  
  ## identifying using arules' Apriori
  #?read.transactions
  sequence <- apply(df, 1 ,paste, collapse=",")
  sequence <- paste(sequence, collapse="\n")
  print(sequence)
  write(sequence, file="sequencedata")
  tr <- read.transactions("sequencedata", format='basket', sep=",")
  file.remove('sequencedata')
  inspect(tr)
  rules <- apriori(tr, parameter=list(supp=0.2))
  inspect(sort(rules))
  
  
  
  ## Calculate duration?
  
  
  
  
  print("#### Output Apriori... ####")
  #return(df)
}