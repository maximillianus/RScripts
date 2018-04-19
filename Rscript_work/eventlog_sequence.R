#### Creating Sequence DataFrame ####

## This is script to create a dataframe of id sequence
## Input: cleaned df

eventlog_sequence <- function(df)
{
  ## Check Top 10 errors then Make into dataframe
  error_df <- subset(df, EventTypeName == 'Error')
  toperrors <- sort(table(error_df$EventSource),decreasing = TRUE)
  if(length(toperrors) > 10)
  {
    toperrors <- toperrors[1:10]
  }
  toperrors <- as.data.frame(toperrors)
  names(toperrors) <- c("EventSource",'Frequency')
  toperrors$EventSource <- as.character(toperrors$EventSource)
  rm(error_df)
  
  ## Defining nth number of event sequence before and after main events
  prev_n = 5
  next_n = 0
  eventseq <- seq(-prev_n, next_n, 1)
  
  ## Initialize main DataFrame
  main_detailseq_df <- data.frame()
  main_seq_df <- data.frame()
  
  ## TO LOOP SNIPPET BELOW
  for(i in 1:nrow(toperrors))
  {
    ## Locate index number of the top errors in original DF
    errorID <- toperrors$EventSource[i]
    indexloc <- which(df$EventSource == errorID)
    
    ## Creating n-th events sequence prior or after the MAIN ERROR
    
    seq_id <- c()
    for(j in 1:toperrors$Frequency[i])
    {
      seq_x <- (indexloc[j]-prev_n):(indexloc[j]+next_n)
      seq_x <- ifelse(seq_x < 1, NA, seq_x)
      seq_id <- append(seq_id, seq_x )
    }
    
    ## Creating dataframe of the detailed event sequences
    temp_detailseq_df <- df[seq_id,c('ServerName', 'Date','Time','DateTime', 'LogType',
                                     'EventID','SourceName','EventSource','EventTypeName' ,
                                     'Description'
    )]
    temp_detailseq_df$EventSequence <- rep(eventseq, toperrors$Frequency[i])
    temp_detailseq_df$Main_error <- errorID
    temp_detailseq_df$Tag <- NA
    temp_detailseq_df$Nth_Occurence <- rep(1:toperrors$Frequency[i], each=length(eventseq))
    temp_detailseq_df[temp_detailseq_df$EventSequence == 0,]$Tag <- 'focal_error'
    
    ## Binding to main_detailseq_df
    main_detailseq_df <- rbind(main_detailseq_df, temp_detailseq_df)
    
    ################################################ 
    
    ## Creating dataframe of event ID sequence
    ## v2.0: Use 'reshape' library
    ## use cast(dataframe, rowindex ~ column)
    ## cast(dataframe, Nth_Occurence ~ EventSequence, value = 'EventSource')
    columnlist <- paste(seq(0-prev_n,0+next_n,1), sep="")
    temp_seq_df <- as.data.frame(matrix(data=NA,
                                        ncol=length(eventseq),
                                        nrow=toperrors$Frequency[i])
    )
    names(temp_seq_df) <- columnlist
    for(j in 1:length(eventseq))
    {
      id = temp_detailseq_df$EventSequence == as.numeric(columnlist[j])
      temp_seq_df[j] <- temp_detailseq_df[id,]$EventID
    }
    rm(id)
    
    temp_seq_df$EventSource=toperrors$EventSource[i]
    temp_seq_df$DateTime=temp_detailseq_df[temp_detailseq_df$EventSequence == 0,]$DateTime
    
    ## Binding to main_seq_df
    main_seq_df <- rbind(main_seq_df, temp_seq_df)
  }
  
  ## END LOOP
  
  ## Create into a list of objects
  listobj <- list(df1=main_detailseq_df, df2=main_seq_df)
  
  ## Return the list
  return(listobj)
}