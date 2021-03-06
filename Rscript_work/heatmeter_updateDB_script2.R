## Rscript to update heat-o-meter database
## Added the last update date into calculation

## Initialize
rm(list=ls())

## Loading Libraries
require(RODBC)


## GETTING INPUT
dbhandle <- odbcDriverConnect('driver={SQL Server};
                                server=SQLSG001SIN0047\\SQLHD02;
                                uid=RUSER;PWD=P@ssw0rd;
                                database=Click2Patch;')
rows <- paste("")
columns <- paste('Server_Name', 'ArticleID', 'Severity', 
                 'Country', 'Last_UPD_Date', sep=', ')

query <- paste("SELECT", rows, columns,
               "FROM [Click2Patch].[dbo].[ServerPatchDetails]"
               #"WHERE", condition, "AND", conditionDate,
               #"ORDER BY", orderCondition
               #"WHERE Last_UPD_Date < '2017-06-01'"
)
print(query)

missinglist <- sqlQuery(dbhandle, query)


## Getting List of All Servers
query2 <- paste("SELECT * FROM [Click2Patch].[dbo].[NonMissingServerPatchDetails]")
allserverlist <- sqlQuery(dbhandle, query2)

## Clean variables
rm(query, query2, columns, rows)

## Cleaning data
names(allserverlist) <- c("Server_Name", "OSVersion", "LastUpdateDate", "Country")
allserverlist$Server_Name <- as.character(allserverlist$Server_Name)
allserverlist$OSVersion <- as.character(allserverlist$OSVersion)
allserverlist$LastUpdateDate <- as.Date(allserverlist$LastUpdateDate)
allserverlist$Country <- as.character(allserverlist$Country)


## Setting Working Directory
setwd("//SVRSG001RPS01.asia.corp.anz.com/pradana1$/My Documents/SGWS-8467837A_notbackedup")
#setwd('D:/DATA01/Projects/Aditya')

## Autoset Working Dir for any computers
## Usage are on hold
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)
#setwd('..')

## Process the data into logic. Output a dataframe.
source('Rscript/calcseverity_v2.R')
severity.df <- calcseverity(missinglist) 


## Process the servers which have no missing patches
# If servername does not contain suffix global.anz.com
# missingserver_name <- gsub("\\.[a-zA-Z0-9]+","",severity.df$Server_Name)
# allserverlist <- allserverlist[order(allserverlist$Server_Name),]
# allserverlist2 <- subset(allserverlist, !(allserverlist$Server_Name %in% missingserver_name) )

# If servername contains suffix global.anz.com
allserverlist <- allserverlist[order(allserverlist$Server_Name),]
allserverlist2 <- subset(allserverlist, !(allserverlist$Server_Name %in% severity.df$Server_Name))

## Adding/Subtracting/Modifying columns to match severity.df column
allserverlist2$MissingPatch <- as.integer(0)
allserverlist2$InitSeverityLevel <- 0
allserverlist2$PatchDateSeverity <- 0
  ## Calculating days since last update date
allserverlist2$DaysSinceLastUpdate <- Sys.Date() - allserverlist2$LastUpdateDate
allserverlist2$DaysSinceLastUpdate <- as.integer(allserverlist2$DaysSinceLastUpdate)

#allserverlist2$Country <- substr(allserverlist2$Server_Name, 4,5)

  ## Calculating severity rate based on days since last update
allserverlist2$PatchDateSeverity <- 0
allserverlist2$PatchDateSeverity <- ifelse(allserverlist2$DaysSinceLastUpdate >= 114, 1, allserverlist2$PatchDateSeverity)
allserverlist2$PatchDateSeverity <- ifelse(allserverlist2$DaysSinceLastUpdate < 114 & 
                                             allserverlist2$DaysSinceLastUpdate >= 38, 0.5, allserverlist2$PatchDateSeverity)
allserverlist2$PatchDateSeverity <- ifelse(is.na(allserverlist2$DaysSinceLastUpdate), 0.5, allserverlist2$PatchDateSeverity)

  ## Calculating severity
allserverlist2$SeverityLevel <- 0
allserverlist2$SeverityLevel <- allserverlist2$SeverityLevel + allserverlist2$PatchDateSeverity

allserverlist2$Timestamp <- as.character(Sys.Date())
allserverlist2$OSVersion <- NULL
allserverlist2$LastUpdateDate <- as.character(allserverlist2$LastUpdateDate)

## Parsing Country Code to Country Name
## **Deprecated since 2017-09-04 due to database adding Country from back-end
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'AU', 'Australia', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'AE', 'United Arab Emirates', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'BC', 'British Columbia', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'CK', 'Cook Islands', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'CN', 'China', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'DE', 'Germany', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'FJ', 'Fiji', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'FR', 'France', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'GB', 'Great Britain', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'HK', 'Hong Kong', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'ID', 'Indonesia', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'IN', 'India', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'KH', 'Cambodia', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'KI', 'Kiribati', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'LA', 'Laos', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'NT', 'Not Applicable', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'NZ', 'New Zealand', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'PG', 'Papua New Guinea', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'PH', 'Philippines', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'SB', 'Solomon Islands', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'SG', 'Singapore', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'TO', 'Tonga', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'TS', 'Not Applicable', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'TW', 'Taiwan', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'US', 'United States of America', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'VN', 'Vietnam', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'VU', 'Vanuatu', allserverlist2$Country)
# allserverlist2$Country <- ifelse(allserverlist2$Country == 'WS', 'Western Samoa', allserverlist2$Country)
## END Parsing


## If servername does not contain .global.anz.com suffix
## Name into .global.anz.com
#allserverlist2$Server_Name <- paste0(allserverlist2$Server_Name, '.GLOBAL.ANZ.COM')

## If servername contains .global.anz.com suffix
# Do nothing

## Combine dataframes: missingpatch + updatedpatch
allserver.df <- rbind(severity.df, allserverlist2)

## INPUT DATA to Database

## Specify tablename and extract list of tables in the database
tablename <- 'ServerPatchUpdateDetails'
listoftables <- sqlTables(dbhandle)

## Check if table exists
if(tablename %in% listoftables$TABLE_NAME)
{
  print(paste("Table",tablename,"exists!"))
  
  ## DROP Table if table exists
  sqlDrop(channel = dbhandle,
          sqtable = paste0('dbo.',tablename),
          errors=TRUE)
  
  ## Create new table and inject with records
  sqlSave(channel = dbhandle,
          dat = allserver.df,
          tablename = paste0('dbo.',tablename),
          rownames=F,
          #append=T
          verbose=T
  )
  
} else {
  print(paste("Table",tablename,"does not exist!"))
  
  ## Create new table and inject with records
  sqlSave(channel = dbhandle,
          dat = allserver.df,
          tablename = paste0('dbo.',tablename),
          rownames=F,
          #append=T
          verbose=T
  )
}

## Close sql connection
print("#### Closing SQL Connection... ####")
odbcClose(dbhandle)

print("#### END Script ####")
