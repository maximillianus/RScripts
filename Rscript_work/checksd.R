checksd <- function(date1 = NA, date2=NA)
{
  ##This is function to check standard deviation of Citrix logon duration data
  ##over a period of time for 7 main states in Australia.
  
  #### Check if date1 & date2 parameter is given, if not, use today date
  if(is.na(date1))
  {
    date1 <- Sys.Date()-1
  }
  
  if(is.na(date2))
  {
    date2 <- Sys.Date()+1
  }
  
  cat('Date range', as.character(date1), ' to ', as.character(date2), '\n')
  #################################################
  
  #### Get the data from database #### (in future maybe replaceable with sqlconn.R script)
  require(RODBC)
  dbhandle <- odbcDriverConnect('driver={SQL Server};
                              server=SQLAU001MEL0220\\DWH;
                              uid=RUSER;PWD=P@ssw0rd;
                              database=DWH;')
  
  ##Making query
  
  query <- paste0("SELECT LogonStartDate, CAST(LogonStartDate as date) as Date, CAST(LogonStartDate as time) as Time, Name, Location, AD_Site, UserName, logonduration ",
  "FROM Citrix_MW_TM_FACT WHERE LogonStartDate >= '", date1,"' AND LogonStartDate < '",date2, "' ",
  "AND Name like 'AU%' and Name not like '%PRE%' AND AD_Site like '%POR%' ",
  "AND LocationCountry = 'Australia' ",
  "ORDER BY LogonStartDate")
                  
  res <- sqlQuery(dbhandle, query)
  res$Date <- as.Date(res$Date)
  res$UserName <- as.character(res$UserName)
  odbcClose(dbhandle)
  #################################################
  
  
  
  ####Std Deviation with outliers ####
  
  stdev <- tapply(res$logonduration, list(res$Location), sd)
  cat('\nStandard Deviation with Outliers: \n')
  print(stdev)
  
  #################################################
  
  ####Std Deviation withOUT outliers ####
  
  outlier <- boxplot.stats(res$logonduration)$out
  res$logonduration2 <- ifelse(res$logonduration %in% outlier, NA, res$logonduration)
  
  stdev2 <- tapply(res$logonduration2, list(res$Location), sd, na.rm=T)
  cat('\nStandard Deviation without Outliers: \n')
  print(stdev2)
  
  
  #################################################
  
  outl <- tapply (res$logonduration, list(res$Location), function(x) {range(boxplot.stats(x)$out)})
  print(outl)
  print(mean(stdev2))
  
  #print(head(res))
  print('END Script')
  
  
}