#### LOG SQL Table Processing ####

## =============================================================== ##
## *This is a script to process and convert raw Log SQL Table to 
## a cleaned, transformed format for analysis.                           
## *This script will take data to SQL Server.
## *DataBase: SCCM_OSD_Log
## *Data previously in raw text format.
##
##
## =============================================================== ##

## Grab data from SQL Server ##

rm(list=ls())

# Making SQL Connection ##

print("Making SQL handle connection...")
require(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};
                               server=SQLSG001SIN0047\\SQLHD02;
                               uid=RUSER;PWD=P@ssw0rd;
                               database=SCCM_OSD_Log;')

logcountry <- 'MEL'
tablename <- paste0("dbo.RAW_OSDLogTable_", logcountry)
query <- paste0("SELECT * FROM ", tablename,
                " WHERE logtext LIKE '%exit code%' and component like 'TSManager%'",
                " ORDER BY machinename, logdate, logtime"
                )
print(query)
res <- sqlQuery(dbhandle, query, as.is=T)
class(res)
str(res)
odbcClose(dbhandle)


## =============================================================== ##


## Dataframe Cleaning & Transformation ##

# Machine Type
# if started with D% - Desktop, if started with L% - Laptop
machinetype <- ifelse(grepl("^D", res$machinename), "Desktop", "Laptop")
res$machinetype <- machinetype

# Error
errorcode <- sub(".*exit code (.*)", "\\1", res$logtext)
res$errorcode <- errorcode

# App Category
# filter 1: remove "Failed to run the action:
appCategory <- sub("^Failed.*?: ", "", res$errormessage)
# filter 2: extract the first sentence (ie. everything before full-stop) containing app name
appCatVersion <- sub("\\. .*", "", appCategory)
# filter 3: remove the version number. remove everything with "space" followed by "digits"
appCategory <- sub("\\s\\d.+", "", appCatVersion)

res$appCategory <- appCategory
res$appCatVersion <- appCatVersion

# Error - AppCat concatenate
errorAppCat <- paste(errorcode, appCategory)

res$errorAppCat <- errorAppCat

# ErrorCause
# filter 1: remove "Failed to run the action:
errorcause <- sub("^Failed.*?: ", "", res$errormessage)
# filter 2: remove last chunk of message containing (Error...)
errorcause <- sub("\\s\\(Err.*$", "", errorcause)
# filter 3: remove first chunk of message containing appCategory
errorcause <- sub(".*?\\.\\s", "", errorcause)
# filter 4: remove last fullstop if any
errorcause <- sub("\\.$", "", errorcause)

res$errorcause <- errorcause



# Date-Time stamp
datestamp <- toString(Sys.Date())
timestamp <- toString(format(Sys.time(), "%T"))
timezone <- toString(format(Sys.time(), "%z"))

res$datestamp <- NULL
res$datestamp <- datestamp
res$timestamp <- NULL
res$timestamp <- timestamp
res$timezone <- NULL
res$timezone <- timezone

# Discard unnecessary column
res$logtext <- NULL


# put into new data frame to upload to SQL
processed_df <- res

## END Clean & Transform ##


## =============================================================== ##


## Uploading data to Database ##
uploadToSQL_toggle <- 1
if(uploadToSQL_toggle)
{
  # Make handle connection
  dbhandle <- odbcDriverConnect('driver={SQL Server};
                                 server=SQLSG001SIN0047\\SQLHD02;
                                 uid=RUSER;PWD=P@ssw0rd;
                                 database=SCCM_OSD_Log;')
  ## Save to SQL Database
  #Create table if table does not exist
  tablename <- paste0('WRK_OSDLogTable_', logcountry)
  #tablename <- 'WRK_OSDLogTable'
  listoftables <- sqlTables(dbhandle)
  
  ## Check if table exists
  if(tablename %in% listoftables$TABLE_NAME)
  {
    print(paste("Table",tablename,"exists!"))
    
    sqlDrop(channel = dbhandle,
            sqtable = tablename)
    
    ## Inject records to existing table
    sqlSave(channel = dbhandle,
            dat = processed_df,
            tablename = paste0('dbo.',tablename),
            rownames=F,
            #append=T,
            fast=F,
            verbose=F
    )
    
  } else {
    print(paste("Table",tablename,"does not exist!"))
    
    ## Create new table and inject with records
    sqlSave(channel = dbhandle,
            dat = processed_df,
            tablename = paste0('dbo.',tablename),
            rownames=F,
            #append=T
            fast=F,
            verbose=F
    )
  }
  odbcClose(dbhandle)
}
## END SQL upload ##


## Finishing script and closing database connection ##
#odbcClose(dbhandle)
