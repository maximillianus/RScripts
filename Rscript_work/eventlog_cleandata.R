#### Event Log Clean Data ####

## This is script to read data from csv for eventlog related files.
## This script will output DataFrame with cleaned data.

eventlog_cleandata <- function(df)
{
  
  ## Parsing Date and Time
  # Check if datetime contains "T" as separator or " "
  if( sum(grepl("T", df$DateTime)) == nrow(df) )
  {
    dfdatetime <- strsplit(df$DateTime, split="T")
    
  } else if( sum(grepl(" ", df$DateTime)) == nrow(df) ){
    
    dfdatetime <- strsplit(df$DateTime, split=" ")
    
  }
  
  df$Date <- sapply(dfdatetime,'[[',1)
  df$Time <- sapply(dfdatetime,'[[',2)
  rm(dfdatetime)
  
  
  # Add hour column
  df$Hour <- paste0(substr(df$Time, 0, 2),":00:00")
  
  # Format Date & Time into Date and POSIXct class
  df$Date <- as.Date(df$Date)
  df$Time <- as.POSIXct(df$Time, format='%H:%M:%S')
  
  # Order based on Date & Time
  df <- df[order(df$Date, df$Time),]
  
  # Join EventID + ServiceType
  df$EventSource <- paste(df$EventID, '-', df$SourceName)
  
  return(df)
}