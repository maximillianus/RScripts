## Rscript to update database

## Make SQL Server connection
require(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};
                              server=SQLSG001SIN0047\\SQLHD02;
                              uid=RUSER;PWD=P@ssw0rd;
                              database=Click2Patch;')

## Query and process data
query = paste0('SELECT Server_Name, Title, ArticleID, Severity, Country FROM dbo.ServerPatchDetails')
missinglist <- sqlQuery(channel = dbhandle, query = query)
##Column: Server_Name, Title, ArticleID, Severity, Country
require(openxlsx)
updatelist <- read.xlsx('datafiles/Security Update-April2016.xlsx', 1, cols=c(3,5,9,10))
##Column: Product, InfoID, Severity, Impact
query2 <- paste0("SELECT [Product], [More Info], FROM dbo.SecurityUpdate-April2016")


source("Rscript/calcseverity_updateDB.R")
listobj <- calcseverity_updateDB('',missinglist,updatelist)

## Drop current Table
sqlDrop(channel = dbhandle,
        sqtable = 'dbo.ServerPatchUpdateDetails',
        errors=TRUE)


## Create new table and inject data
sqlSave(channel = dbhandle,
        dat = listobj$data1,
        tablename = 'dbo.ServerPatchUpdateDetails2',
        rownames=F,
        #append=T
        verbose=T
)