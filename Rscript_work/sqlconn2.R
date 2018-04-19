sqlconn2 <- function(date1=NA, date2=NA, column = '*')
{
  
  ## This is function to connect to database and get the data
  ## In this case, it is connecting to Server Patch Data
  
  
  ################# Check Input ####################
  ## Check if date1 & date2 parameter is given, if not, use today date
  if(is.na(date1))
  {
    date1 <- Sys.Date()-1
  }
  
  ## The end date must be (userInput + 1) for SQL query accuracy
  ## Otherwise sql query must modify to "cast(logonstartdate as date)"
  if(is.na(date2))
  {
    date2 <- Sys.Date()+1
  } else {
    date2 <- as.character(as.Date(date2) + 1)
  }
  
  # cat('Date range', as.character(date1), ' to ', as.character(date2), '\n')
  #################################################
  
  
  ############ Creating ODBC Connection ###########
  require(RODBC)
  dbhandle <- odbcDriverConnect('driver={SQL Server};
                              server=SQLSG001SIN0047\\SQLHD02;
                              uid=RUSER;PWD=P@ssw0rd;
                              database=Click2Patch;')
  
  #################################################
  
  ############ Creating ODBC Connection ###########
  ## Connection to SQLSG001SIN0047\\SQLHD02      ##
  ## Database: Click2Patch                       ##
  ## UID: Ruser ; PWD: P@ssw0rd                  ##
  ## UID: Rpatch ; PWD: P@ssw0rd@123             ##
  #################################################
  
  
  ############### Initial Population ##############
  ## This step is required if we are going to re-populate or
  ## changes the structure of the database
  ## 
  #################################################
  
  ################# Making query ##################
  ##Ver 0.1
  #query = "Select @@Servername as test"
  
  ##Ver 0.2 - fixed query, variable date
  # query <- paste0("SELECT LogonStartDate, CAST(LogonStartDate as date) as Date, CAST(LogonStartDate as time) as Time, Name, Location, AD_Site, UserName, logonduration ",
  #                 "FROM Citrix_MW_TM_FACT WHERE LogonStartDate >= '", date1,"' AND LogonStartDate <= '",date2, "' ",
  #                 "AND Name like 'AU%' and Name not like '%PRE%' AND AD_Site like '%POR%' ",
  #                 "AND LocationCountry = 'Australia' ",
  #                 "ORDER BY LogonStartDate")
  
  ##Ver 0.3 - variable query
  if(column == '*')
  {
    column <- paste('*', 
                    sep='')
  } else {
    column <- paste('ServerNameNoSuffix', 
                    'MissingPatch', 'SeverityLevel',
                    #'Server_Name',
                    sep=', ')
    
  }
  ## QUICK GUIDE
  ## to combine n-length single char vector into 1, use: paste(text1,collapse=', ')
  ## to paste multiple char vectors into 1, use: paste(text1,text2,sep=', ')
  
  #condition <- paste("NAME like 'AU%' and NAME not like '%PRE%' AND AD_Site like '%POR%'",
  #              "AND LocationCountry = 'Australia'")
  #conditionDate <- paste0("LogonStartDate >= '", date1, "' AND ", "LogonStartDate <= '", date2,"'")
  #orderCondition <- paste('LogonStartDate')
  
  query <- paste("SELECT", column,
                 "FROM dbo.ServerPatchDetails",
                 #"WHERE", condition, "AND", conditionDate,
                 "ORDER BY", "ServerNameNoSuffix"
                  )
  print(query)
  #################################################
  
  
  ############ Pre-process data ###################
  res <- sqlQuery(dbhandle, query, max=0)
  cat('Fetch data:\n'); print(res[1:3,])
  cat("Closing ODBC Connection:", odbcClose(dbhandle))

  ##res is database table in form of R dataframe
  #################################################

  
  ############# Print/Return output ###############
  
  cat("\n## END sqlconn.R Script ##\n")
  
  return(res)
  
  ## Ensure that database connection is closed regardless of error
  on.exit(odbcCloseAll())
  #################################################
}