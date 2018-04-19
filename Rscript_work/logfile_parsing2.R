#### LOG File Parsing ####

## =============================================================== ##
## *This is a script to parse unstructured text log to 
## structured table.                           
## *This script will upload data to SQL Server.
## *DataBase: SCCM_OSD_Log
## *Data is previously cleaned from 'CRLF/CR/LF' issue using Python.
## *LogType: BDD
##
## =============================================================== ##


## Reading single log file ##
rm(list=ls())
maindir <- "C:/NotBackedUp/"
workingdir <- "C:/NotBackedUp/datascience_projects/!20180131_OSDLog/"
setwd(workingdir)

## Read File ##
logcountry <- 'SG'
logyear <- '2018'
tgt_dir <- paste0(logcountry, "/", logyear, "/")

#smsts_folder <- "datafiles/taskseq_logfile/smsts_log/cleaned_log"
bdd_folder <- paste0("2 Prepared Data/bdd_cleaned_log/", tgt_dir)
log_folder_list <- dir(bdd_folder)

## Initialize main df to store all rows ##
main_df <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c("machinename", "propertymake", "propertymodel",
       "propertyproduct", "propertymemory" , "propertyarch",
       "propertyprocspeed", "logdate", "logtime")
colnames(main_df) <- x
rm(x)

## Loop through log files ##
logcounter <- 1
for(file in log_folder_list)
{
  print(paste("Iteration: ", logcounter))
  logcounter <- logcounter + 1
  print(file)
  
  fileloc <- paste0(bdd_folder, '/', file)
  lines <- readLines(fileloc)
  
  logtext <- sub(".*LOG\\[(.*?)\\]LOG.*","\\1", lines)
  
  logparam <- sub(".*<(.*?)><(.*?)>","\\2", lines)
  logparam <- tail(logparam, 1)
  
  logparam_time <- sub("time=\"(.*?)\".*", "\\1", logparam)
  logparam_time <- sub("\\..*","",logparam_time)

  logparam_date <- sub(".*date=\"(.*?)\".*", "\\1", logparam)
  logparam_date <- as.character(as.Date(logparam_date, "%m-%d-%Y"))
  
  prop_make <- tail(grep("Property Make", logtext, ignore.case = T, value=T), 1)
  prop_model <- tail(grep("Property Model", logtext, ignore.case = T, value=T), 1)
  prop_product <- tail(grep("Property Product", logtext, ignore.case = T, value=T), 1)
  prop_memory <- tail(grep("Property Memory", logtext, ignore.case = T, value=T), 1)
  prop_arch <- tail(grep("Property Architecture", logtext, ignore.case = T, value=T), 1)
  prop_procspeed <- tail(grep("Property ProcessorSpeed", logtext, ignore.case = T, value=T), 1)
  prop_machinename <- tail(grep("Property HostName", logtext, ignore.case = T, value=T), 1)
  prop_os <- tail(grep("Property OSVersion", logtext, ignore.case = T, value=T), 1)
  prop_error <- (grep("error", logtext, ignore.case = T, value=T))
  
  print(prop_machinename)
  print(prop_model)
  print(prop_product)
  print(unique(prop_error))
  
  main_df[nrow(main_df)+1,] <- c(prop_machinename, prop_make, prop_model, prop_product,
                                 prop_memory, prop_arch, prop_procspeed, logparam_date, logparam_time)
  
}

## Apply sub to each column of dataframe
main_df <- apply(main_df, 2, function(x) {sub(".*= ","",x)})
main_df <- as.data.frame(main_df)


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

## Uploading data to Database
if(uploadToSQL_toggle)
{
  ## Save to SQL Database
  #Create table if table does not exist
  tablename <- 'BDDLogTable'
  listoftables <- sqlTables(dbhandle)
  
  ## Check if table exists
  if(tablename %in% listoftables$TABLE_NAME)
  {
    print(paste("Table",tablename,"exists!"))
    
    ## Inject records to existing table
    sqlSave(channel = dbhandle,
            dat = main_df,
            tablename = paste0('dbo.',tablename),
            rownames=F,
            append=T,
            fast=F,
            verbose=F
    )
    
  } else {
    print(paste("Table",tablename,"does not exist!"))
    
    ## Create new table and inject with records
    sqlSave(channel = dbhandle,
            dat = main_df,
            tablename = paste0('dbo.',tablename),
            rownames=F,
            #append=T
            fast=F,
            verbose=F
    )
  }
  ## END SQL upload
}
odbcClose(dbhandle)

# ## Parsing unstructured text into structured dataframe ##
# logtext <- sub(".*LOG\\[(.*?)\\]LOG.*","\\1", lines)
# ## Log Parameter: read everything between second chevrons <...>
# logparam <- sub(".*<(.*?)><(.*?)>","\\2", lines)
# logparam_time <- sub("time=\"(.*?)\".*", "\\1", logparam)
# logparam_time <- sub("\\..*","",logparam_time)
# 
# logparam_date <- sub(".*date=\"(.*?)\".*", "\\1", logparam)
# logparam_date <- as.character(as.Date(logparam_date, "%m-%d-%Y"))
# 
# logparam_component <- sub(".*component=\"(.*?)\".*", "\\1", logparam)
# logparam_type <- sub(".*type=\"(.*?)\".*", "\\1", logparam)
# logparam_thread <- sub(".*thread=\"(.*?)\".*", "\\1", logparam)
# logparam_file <- sub(".*file=\"(.*?)\".*", "\\1", logparam)
# if(length(lines) > 0)
# {
#   logdf <- data.frame(logtext=logtext, 
#                       logdate=logparam_date, 
#                       logtime=logparam_time, 
#                       component=logparam_component,
#                       type=logparam_type,
#                       thread=logparam_thread,
#                       file=logparam_file,
#                       stringsAsFactors=FALSE)
#   
# } else {
#   logdf <- data.frame(logtext=NA, 
#                       logdate=NA, 
#                       logtime=NA, 
#                       component=NA,
#                       type=NA,
#                       thread=NA,
#                       file=NA,
#                       stringsAsFactors=FALSE)
#   
#   
# }
# 
