calcseverity_updateDB <- function(srvname=character(), missinglist=NA, updatelist=NA)
{
  #### THIS IS SCRIPT TO CALCULATE SEVERITY LEVEL WHEN A SERVER IS MISSING CERTAIN UPDATES
  #### This script will take data from table which contains patch list and missing list
  #### This script is used TO UPDATE TO SQL SERVER DATABASE
  cat('\n\n'); print('####### START Script ########')
  print(srvname)
  print(class(srvname))
  print(head(missinglist,1))
  print(head(updatelist,1))
  
  ######## Read Datafiles & Read User Input ########
  if(is.data.frame(missinglist) == F)
  {
    require(openxlsx)
    # missinglist <- read.xlsx('datafiles/PatchMissing-Global_edited.xlsx', 1, rows=NULL, 
    #                          cols=c(1,4,6,13))
    # 
    missinglist <- read.xlsx('datafiles/PatchMissing-Global_edited.xlsx', 1, rows=NULL, 
                             cols=c('Server_Name','Title','ArticleID','Severity'))
    ##Column: Server_Name, Title, ArticleID, Severity
  } 
  
  if(is.data.frame(updatelist) == F)
  {
    require(openxlsx)
    updatelist <- read.xlsx('datafiles/Security Update-April2016.xlsx', 1, cols=c(3,5,9,10))
    
    ##Column: Product, InfoID, Severity, Impact
  }
  
  ##Check if string contains comma, and separate it
  if(length(srvname)>0 && grepl(',',srvname))
  {
    srvname <- trimws(unlist(strsplit(srvname, split = ',')))
  }
  
  ##Check if input is 0, or '' or servername in approved list
  if(length(srvname) > 0 && srvname != '' && srvname %in% missinglist$Server_Name)
  {
    servername_vector <- srvname
  } else if(length(srvname)>0 && suppressWarnings(is.na(as.numeric(srvname)) ) == FALSE ) ##Is it a number?
  {
    srvnumber <- ifelse(as.numeric(srvname) <= 30, as.numeric(srvname), 30)
    servername_vector <- sample(unique(missinglist$Server_Name),srvnumber)
  } else {
    servername_vector <- sample(unique(missinglist$Server_Name),10)
    ##Get the name for ALL unique servers
    servername_vector <- unique(missinglist$Server_Name)
  }
  #cat("ServerNameList: \n", paste(servername_vector,collapse='\n'), '\n')
  missinglist <- subset(missinglist, Server_Name %in% servername_vector)
  
  ##################################################
  
  
  ################ Pre-Process Data  ################
  missinglist$Severity <- factor(missinglist$Severity,
                                 levels=c('None', 'Low', 'Moderate', 'Important', 'Critical'),
                                 labels=c('None', 'Low', 'Moderate', 'Important', 'Critical'))
  missinglist$Severity.value <- as.numeric(missinglist$Severity)
  
  ##There are some non-numeric value in InfoID, thus forcing them into numeric would
  ##introduce warnings: coercion into NA. Remedy this by making missinglist$Article ID char
  missinglist$ArticleID <- as.character(missinglist$ArticleID)
  
  
  updatelist$Severity <- factor(updatelist$Severity,
                                levels=c('None', 'Low', 'Moderate', 'Important', 'Critical'),
                                labels=c('None', 'Low', 'Moderate', 'Important', 'Critical'))
  updatelist$Severity.value <- as.numeric(updatelist$Severity)
  updatelist$Impact.value <- ifelse(updatelist$Impact %in% c('None'), 1, 5)
  #updatelist$InfoID <- as.numeric(updatelist$InfoID)
  names(updatelist)[2] <- 'ArticleID'
  
  ##Merge both list
  ##Previous code is to merge based on Article ID & Severity. To add Product/OS for more detail
  totallist <- merge(missinglist, updatelist[,c('ArticleID', 'Severity', 'Impact', 'Impact.value')], by=c('ArticleID','Severity'))
  totallist <- unique(totallist)
  cat('\n')
  #print((totallist))
  #print("########################################################################################")
  
  ##Find which ID in missing list is not found in updatelist
  id <- unique(missinglist$ArticleID) %in% unique(updatelist$ArticleID)
  missingID <- unique(missinglist$ArticleID)[id==FALSE]
  
  if(length(missingID)!=0)
  {
    ##if there is missing ID found, rbind the dataframe with totallist dataframe, else mark as NA
    cat('Missing ID:', missingID, '\n')
    missingIDlist <- subset(missinglist, ArticleID %in% missingID)
    missingIDlist$Impact <- NA
    missingIDlist$Impact.value <- 5
    totallist <- rbind(totallist, missingIDlist)
    
  } else {
    print('No Missing Value')
    missingIDlist <- NA
  }
  ##################################################
  
  
  ################ Process Data  ################
  
  ##Calculate total severity * impact ==> not used due to Impact Value is always either 1 or 5
  ##criticalpatch shows the number of patch with severity of 5.
  totallist$totalvalue <- totallist$Severity.value * totallist$Impact.value
  missingpatch <- tapply(totallist$ArticleID, list(totallist$Server_Name), function(x) length(unique(x)))
  severitylevel <- tapply(totallist$Severity.value, list(totallist$Server_Name), mean)
  # 7 July 2017: Critical Patch information is not needed anymore.
  #criticalpatch <- tapply(totallist$Severity.value, list(totallist$Server_Name), function(x) sum(x==5))
  
  
  df <- as.data.frame(cbind(missingpatch, severitylevel))
  df$Server_Name <- rownames(df)
  meanseverity <- mean(df$severitylevel)
  meanpatches <- as.integer(mean(df$missingpatch))
  df$severitylevel <- round(df$severitylevel, digits=2)
  
  ## 7 July 2017: add country to final dataframe
  servercountry <- missinglist[,c(1,5)]
  df <- merge(df, servercountry, by="Server_Name")
  names(df)[2] <- "MissingPatch"
  names(df)[3] <- "SeverityLevel"
  
  df <- unique(df)
  print(dim(df))
  
  
  ##################################################
  
  
  ################ Output Result  ################
  require(ggplot2)
  g <- ggplot(data=df, aes(missingpatch,severitylevel, col=Server_Name))+
       geom_point(size=2)+
       #geom_text(aes(),hjust=0,vjust=2,size=2)+
       xlim(0,40) + ylim(0,5)+
       geom_hline(yintercept=meanseverity, col='green') + geom_text(aes(0,meanseverity, label=as.character(round(meanseverity,2)), vjust=1.3),col='black')+
       geom_vline(xintercept=meanpatches, col='red') + geom_text(aes(meanpatches,0, label=as.character(meanpatches), hjust=-0.8),col='black')
  
  print('####### END Script ########'); cat('\n\n')
  listobj <- list(plot1=g, data1=df, data2=missinglist)
  return(listobj)
  
  ##################################################
  
}



# print('###############Partial Function Execution###############')
#### This portion is to do string extraction for more accurate severity calculation
# missinglist$Product <- missinglist$Title
# 
# #Looking for Skype ONLY Keyword
# skype_keyword <- grep('Skype', missinglist$Product) 
# #Looking for Windows ONLY keyword (perl expr)
# windows_keyword <- grep('^(?!.*NET).*Windows', missinglist$Product, perl=T)
# #Looking for Microsoft ONLY keyword
# office_keyword <- grep('^(?!.*NET).*Windows', missinglist$Product, perl=T) 
# #Looking for NET ONLY keyword
# NET_keyword <- grep('NET', missinglist$Product)
# 
# #Extract Windows Server OS
# #sub('.*Windows', 'Windows',missinglist[]$Prod)
# missinglist[windows_keyword,'Product'] <- sub(' \\(.*','',
#                                           sub('.*Windows', 'Windows', missinglist[windows_keyword,'Product']))
# missinglist[NET_keyword,'Product'] <- sub(' on W.*','',
#                                       sub('.*NET', 'Microsoft .NET', missinglist[NET_keyword,'Product']))
# 
# 
# 
# print(str(missinglist))
# print(missinglist)
# print(missinglist[,'Product'])
# print('###############End Execution###############')
# stop("Stop function")