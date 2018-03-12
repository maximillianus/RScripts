#### LOG File Parsing ####

## =============================================================== ##
## *This is a script to parse unstructured text log to 
## structured table.                           
## *This script will upload data to SQL Server.
## *DataBase: SCCM_OSD_Log
## *Data is previously cleaned from 'CRLF/CR/LF' issue using Python.
## *LogType: SMSTS
##
## =============================================================== ##


## Reading single log file ##

rm(list=ls())
maindir <- "C:/NotBackedUp/"
workingdir <- "C:/NotBackedUp/datascience_projects/!20180131_OSDLog/"
setwd(workingdir)

## Read File ##
logcountry <- 'MEL'
logyear <- '2018'
tgt_dir <- paste0(logcountry, "/", logyear, "/")

#smsts_folder <- "datafiles/taskseq_logfile/smsts_log/cleaned_log"
smsts_folder <- paste0("2 Prepared Data/smsts_cleaned_log/", tgt_dir)
log_folder_list <- dir(smsts_folder)


uploadToSQL_toggle <- 0
## Making SQL Connection ##

if(uploadToSQL_toggle)
{
  print("Making SQL handle connection...")
  require(RODBC)
  dbhandle <- odbcDriverConnect('driver={SQL Server};
                              server=SQLSG001SIN0047\\SQLHD02;
                                uid=RUSER;PWD=P@ssw0rd;
                                database=SCCM_OSD_Log;')
}


## Loop through log files ##
logcounter <- 1
for(file in log_folder_list)
{
  print(paste("Iteration: ", logcounter))
  logcounter <- logcounter + 1
  
  print(file)
  fileloc <- paste0(smsts_folder, '/', file)
  lines <- readLines(fileloc)
  #print(lines[1:3])
  
  ## Parse log file ##
  ## Log Text: read everything between <![LOG[text_to_read]LOG]!>
  logtext <- sub(".*LOG\\[(.*?)\\]LOG.*","\\1", lines)
  
  ## Log Parameter: read everything between second chevrons <...>
  logparam <- sub(".*<(.*?)><(.*?)>","\\2", lines)
  logparam_time <- sub("time=\"(.*?)\".*", "\\1", logparam)
  logparam_time <- sub("\\..*","",logparam_time)
  
  logparam_date <- sub(".*date=\"(.*?)\".*", "\\1", logparam)
  logparam_date <- as.character(as.Date(logparam_date, "%m-%d-%Y"))
  
  logparam_component <- sub(".*component=\"(.*?)\".*", "\\1", logparam)
  logparam_type <- sub(".*type=\"(.*?)\".*", "\\1", logparam)
  logparam_thread <- sub(".*thread=\"(.*?)\".*", "\\1", logparam)
  logparam_file <- sub(".*file=\"(.*?)\".*", "\\1", logparam)
  
  if(length(lines) > 0)
  {
    logdf <- data.frame(logtext=logtext, 
                        logdate=logparam_date, 
                        logtime=logparam_time, 
                        component=logparam_component,
                        type=logparam_type,
                        thread=logparam_thread,
                        file=logparam_file,
                        stringsAsFactors=FALSE)
    
  } else {
    logdf <- data.frame(logtext=NA, 
                        logdate=NA, 
                        logtime=NA, 
                        component=NA,
                        type=NA,
                        thread=NA,
                        file=NA,
                        stringsAsFactors=FALSE)
    
    
  }
  
  machinename <- sub(".*_(.*?)_.*", "\\1", file)
  datestamp <- toString(Sys.Date())
  timestamp <- toString(format(Sys.time(), "%T"))
  timezone <- toString(format(Sys.time(), "%z"))
  logdf$machinename <- machinename
  logdf$datestamp <- datestamp
  logdf$timestamp <- timestamp
  logdf$timezone <- timezone
  logdf$country <- logcountry
  logdf$year <- logyear
  
  #### Regex ####
  # grep("Process.*exit code ([^0]|0x)", logdf$logtext, value=T)
  
  ###############
  
  # capture location of each sequence:
  seq_index <- grep("!.*!", logdf$logtext)
  print(seq_index)
  
  
  # capture location of failure:
  failure_index <- which(logdf$component == 'TSManager' & grepl("exit code [^0]", logdf$logtext))
  print(paste("Failure Index: ", paste(failure_index, collapse=' ')))
  failure_index <- tail(failure_index, n=1)
  print(paste("Last Failure Index: ", paste(failure_index, collapse=' ')))
  
  if(length(failure_index) == 0)
  {
    logdf <- logdf[1,]
    
    logdf$logtext <- NA
    logdf$logdate <- NA
    logdf$logtime <- NA
    logdf$component <- NA
    logdf$type <- NA
    logdf$thread <- NA
    logdf$file <- NA
    logdf$errormessage <- NA
    
    print(logdf)
    
  } else {
    # finding index of task sequence
    matching_idx <- match(failure_index+1, seq_index)-1
    index1 <- seq_index[matching_idx]
    
    # if length of index1 is 0, means last failure index is found
    # at the beginning of seq_index. index1 will have length 0 thus add '1' to index1
    if(length(index1)==0) {index1 <- append(1,index1)}
    index2 <- failure_index+1
    print(index1); print(index2)
    indexes <- c()
    length_index <- c()
    for(i in 1:length(index1))
    {
      # indexes is index for a single sequence of task
      indexes <- append(indexes, index1[i]:index2[i])
      # length_index indicates the number of rows for each sequence
      length_index <- append(length_index, index2[i] - index1[i]+1)
      
    }
    
    # Find out the error message
    error_message <- rep(logdf[failure_index+2,]$logtext, length_index)
    
    # Subset original df to only include error sequence
    logdf <- logdf[indexes,]
    
    # Add the error message itself as part of column
    logdf$errormessage <- error_message
  }
  
  
  
  ## Uploading data to Database
  if(uploadToSQL_toggle)
  {
    ## Save to SQL Database
    #Create table if table does not exist
    if(logcountry == 'SG')
    {
      tablename <- 'RAW_OSDLogTable_SG'
    } else if (logcountry == 'MEL') {
      tablename <- 'RAW_OSDLogTable_MEL'
    }
    
    listoftables <- sqlTables(dbhandle)
    
    ## Check if table exists
    if(tablename %in% listoftables$TABLE_NAME)
    {
      print(paste("Table",tablename,"exists!"))
      
      ## Inject records to existing table
      sqlSave(channel = dbhandle,
              dat = logdf,
              tablename = paste0('dbo.',tablename),
              rownames=F,
              append=T,
              fast=T,
              verbose=F
      )
      
    } else {
      print(paste("Table",tablename,"does not exist!"))
      
      ## Create new table and inject with records
      sqlSave(channel = dbhandle,
              dat = logdf,
              tablename = paste0('dbo.',tablename),
              rownames=F,
              #append=T
              fast=F,
              verbose=F
      )
    }
    ## END SQL upload
  }
  
}
#End Looping

if(uploadToSQL_toggle)
{
  odbcClose(dbhandle)
}


############## END CODE #################

## EXTRA CODE ##

#### Text cleaning due to zero-width-space character in "...[Parsing.."
#### zero-width-space: alt+8203
## Clean the first element of vector from funny character
#logfile[1] <- sub(".*<!","<!",logfile[1])
## Add <![LOG[ to the beginning of those elements without this <![LOG[
#logfile <- ifelse(grepl("^[^<]", logfile), paste0("<![LOG[",logfile), logfile)
## remove elements that only contain "<![LOG[" character
#logfile <- logfile[!grepl("\\[$", logfile)]

# ind <- grep(".*[^>]$", logfile)
# logfile[ind] <- paste0(logfile[ind], logfile[ind+1])
# logfile <- logfile[!grepl("^[^<]", logfile)]



# ## Parse log file ##
# 
# ## Log Text: read everything between <![LOG[text_to_read]LOG]!>
# logtext <- sub(".*LOG\\[(.*?)\\]LOG.*","\\1", logfile)
# 
# ## Log Parameter: read everything between second chevrons <...>
# logparam <- sub(".*<(.*?)><(.*?)>","\\2",logfile)
# logparam_time <- sub("time=\"(.*?)\".*", "\\1", logparam)
# logparam_date <- sub(".*date=\"(.*?)\".*", "\\1", logparam)
# logparam_component <- sub(".*component=\"(.*?)\".*", "\\1", logparam)
# logparam_type <- sub(".*type=\"(.*?)\".*", "\\1", logparam)
# logparam_thread <- sub(".*thread=\"(.*?)\".*", "\\1", logparam)
# logparam_file <- sub(".*file=\"(.*?)\".*", "\\1", logparam)
# 
# #### Create dataframe out of logfile to get structured data
# logdf <- data.frame(logtext=logtext, 
#                     date=logparam_date, 
#                     time=logparam_time, 
#                     component=logparam_component,
#                     type=logparam_type,
#                     thread=logparam_thread,
#                     file=logparam_file,
#                     stringsAsFactors=FALSE)
# logdf <- logdf[1:10,]

