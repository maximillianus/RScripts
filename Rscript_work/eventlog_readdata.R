#### Event Log Reading Data ####

## This is script to read data from csv for eventlog related files.
## This script will output parsed file as DataFrame.

eventlog_readdata <- function(fileloc)
{
  ## Validate file location exists and is a string
  if( missing(fileloc) || (is.character(fileloc)==FALSE) )
  {
    stop("Please input a proper file location")
  } else {
    cat("Your File:", fileloc,"\n", sep="")
  }
  
  ## Read 2 rows first and count number of columns
  ## This is to check whether file has 7 or 15 cols
  df = read.csv(fileloc, as.is=TRUE, nrows=2)
  
  if(ncol(df) == 7)
  {
    
    print("***HyperV Logs***")
    if( names(df)[1] != "EventID" )
    {
      df = read.csv(fileloc, as.is=TRUE, header=FALSE, nrows=-1)
      names(df) <- c('EventID', 'LogType', 'ServerName', 'Description',
                     'SourceName','DateTime','EventTypeName')
    } else {
      df = read.csv(fileloc, as.is=TRUE, header=TRUE, nrows=-1)
      names(df) <- c('EventID', 'LogType', 'ServerName', 'Description',
                     'SourceName','DateTime','EventTypeName')
    }
    
  } else if(ncol(df) == 15) {
    
    print("***Workstation Logs***")
    df = read.csv(fileloc, as.is=TRUE, header=TRUE, nrows=-1, skip=0)
    
    ## DEPRECATED as of 20-Oct-2017
    # For individual logfile, find whether it's App or Sys
    # extract filename from full path
    #logtype <- sub('.*/', '\\1', fileloc)
    # extract only logtype name : system/app/security
    #logtype <- sub('.*_', '\\1', logtype)
    #logtype <- sub('.*(System|Application).*', '\\1', logtype)
    #logtype <- sub('\\..*', '\\1', logtype) #remove extension
    
    ## NEW as of 20-Oct-2017
    logtype <- tools::file_path_sans_ext(basename(df$EventLog))
    logtype <- sub('.*(System|Application).*', '\\1', logtype)
    
    
    df$LogType <- logtype
    
    eventtype <- strsplit(df$EventTypeName, split=" ")
    df$EventTypeName <- sapply(eventtype,'[[',1)
    
    selectioncol <- c("EventID","LogType", "ComputerName", "Message",
                      "SourceName","TimeWritten","EventTypeName")
    #df <- df[ , !(names(df) %in% drops)]
    df <- subset(df ,select=selectioncol)
    names(df) <- c('EventID', 'LogType', 'ServerName', 'Description',
                   'SourceName','DateTime','EventTypeName')
    
    rm(logtype, selectioncol)
  }
  
  return(df)
}