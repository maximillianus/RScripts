#############################################################
################## CALCSEVERITY.R Version 2 #################
#### This is a version 2.0 script. It considers server's ####
#### last patch-update date to calculate the severity.   ####
#### It will then update the data to database            ####
#############################################################



calcseverity <- function(missinglist=NA)
{
  #*********************************************************************#
  #************************** NEW SCRIPT *******************************#
  #*********************************************************************#
  
  cat('\n\n'); print('####### START Script ########')
  
  
  
  
  #### PRE-PROCESS DATA ####
  ## Cleaning data types
  missinglist$Severity <- factor(missinglist$Severity,
                                 levels=c('None', 'Low', 'Moderate', 'Important', 'Critical'),
                                 labels=c('None', 'Low', 'Moderate', 'Important', 'Critical'))
  missinglist$Severity.value <- as.numeric(missinglist$Severity)
  missinglist$Last_UPD_Date <- as.Date(missinglist$Last_UPD_Date)
  missinglist$Country <- as.character(missinglist$Country)
  missinglist$Server_Name <- as.character(missinglist$Server_Name)
  
  #### PROCESS DATA ####
  #missingpatch <- tapply(missinglist$ArticleID, list(missinglist$Server_Name), function(x) length(unique(x)))
  missingpatch <- tapply(missinglist$ArticleID, list(missinglist$Server_Name), length)
  severitylevel <- tapply(missinglist$Severity.value, list(missinglist$Server_Name), mean)
  
  
  ## if last patch date is: >90d, severity+1. >30d & <90d severity+0.5. <30d severity+0.
  lastpatchdate <- unique(missinglist[,c("Server_Name", "Last_UPD_Date", "Country")])
  lastpatchdate <- lastpatchdate[order(lastpatchdate$Server_Name),]
  lastpatchdate$dayssincelastupd <- Sys.Date()- lastpatchdate$Last_UPD_Date
  lastpatchdate$patchseverityvalue <- 0
  lastpatchdate$patchseverityvalue <- ifelse(lastpatchdate$dayssincelastupd >= 114, 1, lastpatchdate$patchseverityvalue)
  lastpatchdate$patchseverityvalue <- ifelse(lastpatchdate$dayssincelastupd < 114 & 
                                               lastpatchdate$dayssincelastupd >= 38, 0.5, lastpatchdate$patchseverityvalue)
  lastpatchdate$patchseverityvalue <- ifelse(is.na(lastpatchdate$Last_UPD_Date), 0.5, lastpatchdate$patchseverityvalue)
  
  
  ## create into data frame
  severity.df <- data.frame(Server_Name=names(missingpatch), 
                            MissingPatch=missingpatch,
                            InitSeverityLevel=round(severitylevel, digits=2), 
                            LastUpdateDate=lastpatchdate$Last_UPD_Date,
                            PatchDateSeverity=lastpatchdate$patchseverityvalue,
                            DaysSinceLastUpdate=lastpatchdate$dayssincelastupd,
                            Country = lastpatchdate$Country,
                            stringsAsFactors = F)
  
  ## calculate final server's patch severity value
  severity.df$SeverityLevel <- severity.df$InitSeverityLevel + severity.df$PatchDateSeverity
  
  ## if value > 5, round down to 5
  severity.df$SeverityLevel <- ifelse(severity.df$SeverityLevel > 5, 5, severity.df$SeverityLevel)
  
  
  ## Add few more columns for data clarity
  #timestamp
  severity.df$Timestamp <- Sys.Date()
  
  ## Clean the dataframe (remove dimnames, change datatype, etc.)
  
  dimnames(severity.df$MissingPatch) <- NULL
  dimnames(severity.df$InitSeverityLevel) <- NULL
  dimnames(severity.df$SeverityLevel) <- NULL
  
  #Change datatype to easily inject to SQL Server
  severity.df$LastUpdateDate <- as.character(severity.df$LastUpdateDate)
  severity.df$Timestamp <- as.character(severity.df$Timestamp)
  severity.df$DaysSinceLastUpdate <- as.integer(severity.df$DaysSinceLastUpdate)
  
  
  print('####### END Script ########'); cat('\n\n')
  return(severity.df)

  ##################################################
  
}

