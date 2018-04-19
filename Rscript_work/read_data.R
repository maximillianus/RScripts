########## STARTING RSTUDIO SCRIPT ##########
#.libPaths('C:/Users/pradana1/home/R/win-library/3.3')
#.libPaths('C:/NotBackedUp/aditya/R/win-library/3.4')
#.libPaths('C:/NotBackedUp/R/win-library/3.4')
#.libPaths('C:/NotBackedUp/R/win-library/3.4.1')
#
#############################################
#serverdir <- "\\\\SVRSG001RPS01.asia.corp.anz.com\\pradana1$\\My Documents\\SGWS-8467837A_notbackedup\\Rscript"
#setwd()


############# Finding Files #################

filelist <- grep("Security.evtx", list.files('C:/NotBackedUp/datafiles',recursive = T),value=T, ignore.case = T)
tempwd <- getwd()
setwd("C:/NotBackedUp")
fileloc <- paste0(getwd(), '/datafiles/', filelist[5])
setwd(tempwd)
fileloc

#############################################




##read data:
df <- data.frame(servername = rep('server1',5),
                 severity.value = c(4,4,5,5,1))


##Read Australia Data and map the geolocation

require(openxlsx)

postcodedata <- read.csv('datafiles/Australian_Post_Codes_Lat_Lon.csv', 1)
adsitemap <- read.xlsx('datafiles/AD_Sites_mapping.xlsx', 3)
names(adsitemap)[5] <- 'postcode'
merged <- merge(postcodedata, adsitemap[,c('Site','SubLocation','postcode')], by='postcode')

##merge with citrix data
merged <- merge(df2.week2, merged[,c('AD_Site', 'lat','lon','SubLocation')], by='AD_Site')

##Create string
locstring <- paste(adsitemap$Location, 
                   'postal code', 
                   adsitemap$postcode, 
                   adsitemap$SubLocation)

##String for postcode that is NA
locstring2 <- ifelse(is.na(NApostcode$`City/Town`) == TRUE,
                     paste(NApostcode$Country,
                           NApostcode$Location,
                           NApostcode$SubLocation),
                     paste(NApostcode$Country,
                           NApostcode$Location,
                           NApostcode$`City/Town`,
                           NApostcode$SubLocation)
                      )


##String for postcode that is not NA but lonlat is NA
locstring2 <- ifelse(is.na(NApostcode$`City/Town`) == TRUE,
                     paste(NApostcode$Country,
                           NApostcode$Location,
                           NApostcode$SubLocation),
                     paste(NApostcode$Country,
                           NApostcode$Location,
                           NApostcode$`City/Town`,
                           NApostcode$SubLocation)
)


locstring3 <- paste(notNApostcode$Country,
                    notNApostcode$Location,
                    notNApostcode$`City/Town`,
                    notNApostcode$SubLocation)

locstring4 <- paste(notNApostcode[is.na(notNApostcode$lon)==T,]$Country,
                    notNApostcode[is.na(notNApostcode$lon)==T,]$Location,
                    notNApostcode[is.na(notNApostcode$lon)==T,]$SubLocation)


##READ AND GEOCODE DATA FOR THE REST OF AD-SITES
require(ggmap)
lonlat <- geocode(locstring)
adsitemap$lon <- lonlat$lon
adsitemap$lat <- lonlat$lat

##Applying geocodes for NA postcode
lonlat2 <- geocode(locstring2)

##Applying geocodes for non NA postcode but NA lonlat
lonlat3 <- geocode(locstring3)
notNApostcode[is.na(notNApostcode$lon)==T,]$lon <- lonlat4$lon
notNApostcode[is.na(notNApostcode$lat)==T,]$lat <- lonlat4$lat


##Subsetting
##Subset for NA postcodes
adsitemap[rowindex,c('Country','Location', 'SubLocation','postcode', 'City/Town')]

##Subset for NA


##Geocoding all ADSites:
sitestring <- paste(adsite$Country, adsite$Location, adsite$DetailLoc, adsite$SubLocation, sep=', ')

locstring <- with(testadsite, paste(Country,Location,SubLocation,sep=','))
locstring <- with(testadsite2, paste(Country,Location,DetailLoc,sep=','))
x <- geocode(locstring)

adsite[adsite$Country == 'country',]$lon <- x$lon
adsite[adsite$Country == 'country',]$lat <- x$lat

##Subsetting country:
##With NA ONLY
adsite[adsite$Country=='Papua New Guinea' & (is.na(adsite$lon)==T | is.na(adsite$lat)==T),]

##ALL
adsite[adsite$Country=='Papua New Guinea',]


##Drawing all the fonts
xpos <- rep(seq(1,18),each=10)[1:178]
ypos <- rep(seq(1,10),18)[1:178]
df2 <- data.frame(x=xpos, y=ypos, font=fonts())
ggfont <- ggplot(df2, aes(x=x, y=y)) + geom_text(aes(label='HEATMETER', family=font)) + 
          geom_text(aes(label=font, family=font, vjust=1.3)) 
ggsave('font.png', device='png',width=30, height=15, dpi=150)



### SQL READING, UPDATING, CLEARING, CREATING OPERATIONS

## SQL MAKING CONNECTION
require(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};
                              server=SQLSG001SIN0047\\SQLHD02;
                              uid=RUSER;PWD=P@ssw0rd;
                              database=Click2Patch;')

## SQL CLEAR ROWS
sqlClear(channel = dbhandle,
         sqtable = 'dbo.ServerMissingPatchList',
         errors=TRUE)

## SQL DROP TABLE
sqlDrop(channel = dbhandle,
        sqtable = 'dbo.ServerPatchUpdateDetails',
        errors=TRUE)

## SQL UPDATE ROWS
sqlUpdate(channel = dbhandle,
          dat = (df3),
          tablename = 'dbo.ServerPatchUpdateDetails',
          index = 'Server_Name',
          verbose=T,
          test=F,
          fast=T
          )

sqlUpdate(channel = dbhandle,
          dat = (df1),
          tablename = 'dbo.ServerPatchUpdateDetailsCopy2',
          index = 'Server_Name',
          verbose=T,
          test=F,
          fast=T
)

## SQL INSERT INTO TABLE
sqlSave(channel = dbhandle,
        dat = df3,
        tablename = 'dbo.ServerPatchUpdateDetails',
        rownames=F,
        #append=T
        verbose=T
        )

## APPEND TO EXISTING TABLE
sqlSave(channel = dbhandle,
        dat = (missinglist),
        tablename = 'dbo.ServerMissingPatchList',
        rownames=F,
        append=T
)

## CREATE AND INSERT NEW TABLE
sqlSave(channel = dbhandle,
        dat = (df2),
        tablename = 'dbo.ServerMissingPatchList',
        rownames=F,
        verbose=T
        )

## MAKING QUERY
query <- paste0('UPDATE GLOBALServerPatchDetails ',
               "SET ServerName = '", (df2$ServerName),
               "', missingpatch = '", df2$missingpatch,
               "', severitylevel = '", df2$severitylevel,
               "'")
query <- paste0('SELECT DISTINCT Server_Name,Last_UPD_Date,Country from dbo.ServerPatchDetails')
query <- paste0('SELECT DISTINCT Server_Name from dbo.ServerPatchUpdateDetails')
query <- paste0("SELECT * FROM dbo.ServerPatchDetails")


## ALTER table and ADD Column
query <- paste("ALTER TABLE dbo.ServerPatchUpdateDetails ADD Country varchar(100)")

##INSERT Data to 1 column only
query <- paste("INSERT INTO dbo.ServerPatchUpdateDetails (Server_Name)",
               "VALUES ('APPAU001MEL0579.AU.OCEANIA.INTRANET'), ('APPAU001MEL0621.OCEANIA.CORP.ANZ.COM')")

##INSERT DATA to 1 column only using string concatenation
query <- paste("INSERT INTO dbo.ServerPatchUpdateDetails (Server_Name)",
               "VALUES", srvname)


sqlQuery(channel = dbhandle, query = query)

#'', '', '','','')")



##############################

## QUERY INTO SQL
sqlQuery(channel = dbhandle,
         query = query)

servername <- sqlQuery(channel = dbhandle, query = query)


.libPaths('C:/Users/pradana1/home/R/win-library/3.3')
source('Rshiny/severity_app/helper/calcseverity.R')
listobj <- calcseverity('',missinglist,updatelist)
df <- listobj$data1
names(df)[1] <- 'MissingPatch'
names(df)[2] <- 'SeverityLevel'
names(df)[3] <- 'ServerNameNoSuffix'
df <- df[c('servername','missingpatch','severitylevel')]
names(df)[1] <- 'ServerName'

which(df$ServerName == 'APPAU020HWT395')
df2 <- df[462,]
df2 <- df[which(df$ServerName == 'BLDIN003BLR0003'),]

servername <- servername[c('Server_Name','ServerNameNoSuffix','missingpatch','severitylevel')]




#### SPEED UP HEATMETER CREATION ####
df <- data.frame(val = seq(0,4.95,0.05), tex=sample(LETTERS,100,replace=T))

system.time(lista <- apply(df,1,function(x){heatmeter(as.numeric(x[1]), x[2])}))
system.time(grobj <- arrangeGrob(grobs=lista, nrow=ceiling(length(lista)/2),ncol=2))
g <- grid.arrange(grobj)

f <- function()
{
  rnorm(1e6)
  5
}

##Method to concatenate server name
srvname <- paste0("'",df2[server_id,]$Server_Name,"'",collapse=',')

srvname <- paste0("('",df3[3:131,]$Server_Name,"')",collapse=', ')


######## HEATMAP WORKFLOW for 2 WEEKS ########
##Set working dir to Online Folder
rm(list=ls())
setwd("//SVRSG001RPS01.asia.corp.anz.com/pradana1$/My Documents/SGWS-8467837A_notbackedup")
## SQL connection to get data for 2 weeks
source('Rscript/sqlconn3_newserver.R')
backdate <- Sys.Date()-13
df <- sqlconn(backdate)
#df <- sqlconn(date1='2018-01-26', date2='2018-02-09')

#df <- sqlconn(backdate+1, backdate+2)
## Calculate Data for 2 weeks
source('Rscript/calcdifflogon.R')
calcdifflogon(df,'week','mean')
calcdifflogon(df,'week','median')
calcdifflogon(df,'week','sd')
##############################################
calcdifflogon(df,'day','mean')
calcdifflogon(df.mw,'day','mean')
calcdifflogon(df.tm,'day','mean')


##############################################

########## HEATMAP DATA CHECKER ##############
df <- subset(df, logonduration > 0)

#df.mw <- df[df$Name == 'AU MW BRANCH NP WIN7 DESKTOPS_PROD',]
#df.tm <- df[df$Name == 'AU TM BRANCH NP WIN7 DESKTOPS_PROD',]


df.prevday <- subset(df, Date >= max(df$Date)-13 & Date <= max(df$Date)-7)
df.currday <- subset(df, Date >= max(df$Date)-6 & Date <= max(df$Date))

df.currday[df.currday$logonduration>0,]
df.currdayNT <- subset(df.currday, Location == 'Northern Territory')
df.currdayVic <- subset(df.currday, Location == 'Vic')
df.currdayNSW <- subset(df.currday, Location == 'New South Wales')
df.currdayWA <- subset(df.currday, Location == 'Western Australia')
df.currdaySA <- subset(df.currday, Location == 'South Australia')
df.currdayQN <- subset(df.currday, Location == 'Queensland')

df.currday[df.currday$UserName =='crossv',]
meancurr <- tapply(df.currday[df.currday$logonduration>-1,]$logonduration, 
                   list(df.currday[df.currday$logonduration>-1,]$Location), 
                   sd)

barplot(tapply(df.currdayNSW$logonduration, df.currdayNSW$UserName, sd))
barplot(tapply(df.currdayNSW$logonduration, df.currdayNSW$AD_Site, sd))
barplot(tapply(df.currdayNSW$logonduration, df.currdayNSW$Date, sd))
#x <- df.currday[df.currday$logonduration>1000,]
#Calculate how many logon times per username that is bigger than 500
#Then sort the table, then convert into dataframe
as.data.frame(sort(table(df.currday[df.currday$logonduration>1000,]$UserName), decreasing=T))

df.currdaynt <- subset(df.currday, Location == 'Northern Territory')
df.currdaysa <- subset(df.currday, Location == 'South Australia')
sd(df.currdaywa[df.currdaywa$logonduration<100,]$logonduration)
df.currdaywa[df.currdaywa$logonduration>200,]


##############################################

#### Citrix High logon duration analysis ####

# Getting the name for all users which has logon duration > 120secs
# tells us if there is probably patch applied to laptop and slows logon duration
df.currday120 <- df.currday[df.currday$logonduration >= 120,]
library(dplyr)
# Filter all users who has 120secs logtime more than 1x then count by location
df.currday120 %>% count(Location)
df.currday120 %>% group_by(UserName) %>% filter(n()>1) %>% ungroup() %>% count(Location)

# No of unique Users who have logon duration < 120s
length(unique(df.currday$UserName[df.currday$logonduration<120]))
# No of unique Users who have logon duration > 120s
length(unique(df.currday$UserName[df.currday$logonduration>120]))

## Differentiating between user with logon > 120 and logon < 120
userall <- unique(df.currday$UserName)
user120 <- unique(df.currday$UserName[df.currday$logonduration>120])
usernot120 <- setdiff(userall, user120)

#Getting all users which has logontime >120secs more than 1x
usernamelist <- table(df.currday120$UserName)[table(df.currday120$UserName)>1]

# Getting the AD Site for all sites which has logon duration > 120secs
# tells us if there is network issue

#serverdir <- "\\\\SVRSG001RPS01.asia.corp.anz.com\\pradana1$\\My Documents\\SGWS-8467837A_notbackedup\\Rscript"
setwd("\\\\SVRSG001RPS01.asia.corp.anz.com\\pradana1$\\My Documents\\SGWS-8467837A_notbackedup")

source('Rscript/sqlconn_deepdive.R')
backdate <- Sys.Date()-13
#df_deepdive <- sqlconn_dd(backdate)
df_deepdive <- sqlconn_dd(date1='2017-12-21', date2='2017-12-21')

df_deepdive <- subset(df_deepdive, logonduration > 0)
dfdd.prevweek <- subset(df_deepdive, Date >= max(df_deepdive$Date)-13 & Date <= max(df_deepdive$Date)-7)
dfdd.currweek <- subset(df_deepdive, Date >= max(df_deepdive$Date)-6 & Date <= max(df_deepdive$Date))
dfdd.currweek120 <- dfdd.currweek[dfdd.currweek$logonduration>=120,]
username120 <- unique(dfdd.currweek120$UserName)

nrow(unique(dfdd.currweek120[,c("LoginId", "HostingServerName", "UserName")]))

write.csv((unique(dfdd.currweek120[,c("LoginId", "HostingServerName", "UserName")])), file="uniqueVDIlist.csv", row.names = F)

## Checking on hostname comparison
hostservermean_curr <- tapply(dfdd.currweek$logonduration, dfdd.currweek$HostingServerName, mean)
hostservermean_prev <- tapply(dfdd.prevweek$logonduration, dfdd.prevweek$HostingServerName, mean)
hostservermean_curr <- as.data.frame(hostservermean_curr)
hostservermean_curr$servername <- row.names(hostservermean_curr)
hostservermean_prev <- as.data.frame(hostservermean_prev)
hostservermean_prev$servername <- row.names(hostservermean_prev)
hostservermean_compare <- merge(hostservermean_curr, hostservermean_prev, by="servername")
hostservermean_compare$diff <- hostservermean_compare$hostservermean_curr - hostservermean_compare$hostservermean_prev
hostservermean_compare <- hostservermean_compare[order(-hostservermean_compare$diff),]
hostserverlength_curr <- tapply(dfdd.currweek$logonduration, dfdd.currweek$HostingServerName, length)
hostserverlength_prev <- tapply(dfdd.prevweek$logonduration, dfdd.prevweek$HostingServerName, length)
hostserverlength_curr <- as.data.frame(hostserverlength_curr)
hostserverlength_curr$servername <- row.names(hostserverlength_curr)
hostserverlength_prev <- as.data.frame(hostserverlength_prev)
hostserverlength_prev$servername <- row.names(hostserverlength_prev)
hostservercompare <- merge(hostservermean_compare, hostserverlength_curr, by='servername')
hostservercompare <- merge(hostservercompare, hostserverlength_prev, by='servername')
hostservercompare <- hostservercompare[order(-hostservercompare$diff),]

## Check by user who has logon >= 120s
df_deepdive$Week <- ifelse(df_deepdive$Date >= max(df_deepdive$Date)-13 & df_deepdive$Date <= max(df_deepdive$Date)-7, 'Last', 'Curr')
df_user120 <- subset(df_deepdive, UserName %in% username120)
df_usernot120 <- subset(df_deepdive, !UserName %in% username120)
library(dplyr)
usertimecompare <- df_user120 %>% group_by(UserName, Week) %>% summarise(ProfileLoadmean=mean(ProfileLoadTimeMS),
                                                      LogonScriptmean=mean(LogonScriptTimeMS),
                                                      HDxTimemean=mean(HDxTimeMS),
                                                      AuthTimemean=mean(AuthenticationDurationMS),
                                                      GpoTimemean=mean(GpoTimeMS),
                                                      Interactivemean=mean(InteractiveTimeMS),
                                                      BrokerDurationmean=mean(BrokeringDurationMS),
                                                      SumTime=sum(ProfileLoadTimeMS,LogonScriptTimeMS, HDxTimeMS, AuthenticationDurationMS, GpoTimeMS, InteractiveTimeMS, BrokeringDurationMS)/1000)

prob_user <- df_user120 %>% group_by(Week) %>% summarise(ProfileLoadmean=mean(ProfileLoadTimeMS),
                                                      LogonScriptmean=mean(LogonScriptTimeMS),
                                                      HDxTimemean=mean(HDxTimeMS),
                                                      AuthTimemean=mean(AuthenticationDurationMS),
                                                      GpoTimemean=mean(GpoTimeMS),
                                                      Interactivemean=mean(InteractiveTimeMS),
                                                      BrokerDurationmean=mean(BrokeringDurationMS),
                                                      LogonDurationmean=mean(logonduration))

prob_user_melt <- melt(prob_user, id=c("Week"))
prob_user_melt <- prob_user_melt %>% 
  group_by(variable) %>% 
  mutate(pctgValue=value/lead(value, default=value[2])) %>% 
  ungroup()

prob_user_melt$pctgValue <- round(prob_user_melt$pctgValue, digits=2)
prob_user_melt$Week <- ifelse(prob_user_melt$Week == 'Curr', 'CurrWeek', 'LastWeek')

nonprob_user <- df_usernot120 %>% group_by(Week) %>% summarise(ProfileLoadmean=mean(ProfileLoadTimeMS),
                                            LogonScriptmean=mean(LogonScriptTimeMS),
                                            HDxTimemean=mean(HDxTimeMS),
                                            AuthTimemean=mean(AuthenticationDurationMS),
                                            GpoTimemean=mean(GpoTimeMS),
                                            Interactivemean=mean(InteractiveTimeMS),
                                            BrokerDurationmean=mean(BrokeringDurationMS),
                                            LogonDurationmean=mean(logonduration))

nonprob_user_melt <- melt(nonprob_user, id=c("Week"))
nonprob_user_melt <- nonprob_user_melt %>% 
  group_by(variable) %>% 
  mutate(pctgValue=value/lead(value, default=value[2])) %>% 
  ungroup()

nonprob_user_melt$pctgValue <- round(nonprob_user_melt$pctgValue, digits=2)
nonprob_user_melt$Week <- ifelse(nonprob_user_melt$Week == 'Curr', 'CurrWeek', 'LastWeek')


## Plotting
library(ggplot2)
ggplot(usertimecompare[1:1000,]) + 
  geom_bar(aes(x=UserName, y=GpoTimemean, fill=Week), stat='identity', position = 'dodge', width=0.7) + 
  #theme(axis.text.x = element_text(angle = 90, hjust=0.5)) + 
  coord_flip()

ggplot(data=prob_user_melt, aes(x=variable, y=pctgValue, fill=Week)) + 
  geom_bar(stat='identity', position = 'dodge') + 
  geom_text(aes(label=pctgValue),size=3, position=position_dodge(width=0.9), vjust=-0.5 ) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  ggtitle("Time Stats for Users with Logon >= 120")
  
ggplot(data=nonprob_user_melt, aes(x=variable, y=pctgValue, fill=Week)) + 
  geom_bar(stat='identity', position = 'dodge') + 
  geom_text(aes(label=pctgValue),size=3, position=position_dodge(width=0.9), vjust=-0.5 ) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  ggtitle("Time Stats for Users with Logon < 120")



##################################################################

######## HEATMAP WORKFLOW for 2 MONTHS ########
## SQL Connection to get data for 2 months
setwd("//SVRSG001RPS01.asia.corp.anz.com/pradana1$/My Documents/SGWS-8467837A_notbackedup")
source('Rscript/sqlconn.R')
df <- sqlconn('2017-12-21', '2017-12-21')

## Calculate data for 2 months
source('Rscript/calcdifflogon_monthly.R')
calcdifflogon(df,'month','mean')
calcdifflogon(df,'month','median')
calcdifflogon(df,'month','sd')

##############################################


###############################
######## VDI ANALYSIS #########
###############################

rm(list=ls())
setwd("C:/NotBackedUp/")
## FOR XLSX
library(openxlsx)
vdi_list_MW <- read.xlsx("datafiles/EnableAV_18122017.xlsx", sheet = 1, cols = 1)
vdi_list_TM <- read.xlsx("datafiles/EnableAV_18122017.xlsx", sheet = 2, cols = 1)
vdi_list_MW$Branch <- 'MW'
vdi_list_TM$Branch <- 'TM'
vdi_list <- rbind(vdi_list_MW, vdi_list_TM)
names(vdi_list) <- c("VDI_Name", "VDI_Branch")

#FOR CSV
vdi_list <- read.csv("datafiles/vdiMWTM_list_combined.csv", stringsAsFactors = F)
vdi_list$MW.VDI <- toupper(vdi_list$MW.VDI)
vdi_list$MW.VDI <- trimws(vdi_list$MW.VDI)
vdi_list$TM.VDI <- toupper(vdi_list$TM.VDI)
vdi_list$TM.VDI <- trimws(vdi_list$TM.VDI)

library(reshape2)
vdi_list <- melt(vdi_list, id.vars = c())
vdi_list$variable <- gsub(".VDI", "", vdi_list$variable)
vdi_list$VDI_branch <- vdi_list$variable
vdi_list$variable <- NULL
names(vdi_list) <- c("VDI_Name", "VDI_Branch")
vdi_list <- unique(vdi_list)

### --------------------------------------------- ###

vdiMW <- vdi_list$VDI_Name[vdi_list$VDI_Branch == "MW"]
vdiMW_query <- paste0("(", paste0("'", "GLOBAL\\", vdiMW, "'", collapse = ','), ")")
vdiTM <- vdi_list$VDI_Name[vdi_list$VDI_Branch == "TM"]
vdiTM_query <- paste0("(", paste0("'", "GLOBAL\\", vdiTM, "'", collapse = ','), ")")
vdi_list_vector <- c(vdiMW, vdiTM)
vdi_query <- paste0("(", paste0("'", "GLOBAL\\", vdi_list_vector, "'", collapse = ','), ")")

### --------------------------------------------- ###

require(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};
                              server=SQLAU001MEL0220\\DWH;
                              uid=RUSER;PWD=P@ssw0rd;
                              database=DWH;')
column <- paste('LogonStartDate', 'CAST(LogonStartDate as date) as Date',
                'CAST(LogonStartDate as time) as Time', 'Name', 'Location',
                'AD_Site', 'UserName', 'logonduration', 'HDxTimeMS', 'AuthenticationDurationMS',
                'GpoTimeMS', 'ProfileLoadTimeMS', 'InteractiveTimeMS',
                'LogonScriptTimeMS', 'BrokeringDurationMS', 
                'LoginId', 'HostingServerName', sep=', ')
condition <- paste("LoginId IN", vdi_query)
date1 <- "2017-12-11"
date2 <- "2017-12-23"
conditionDate <- paste0("LogonStartDate >= '", date1, "' AND ", "LogonStartDate <= '", date2,"'")
orderCondition <- paste('LogonStartDate')
query <- paste("SELECT", column,
               "FROM Citrix_MW_TM_FACT",
               "WHERE", condition, "AND", conditionDate,
               "ORDER BY", orderCondition
              )
print(query)
res <- sqlQuery(dbhandle, query)
res$Date <- as.Date(res$Date)
res$UserName <- as.character(res$UserName)
res$LoginId <- as.character(res$LoginId)
res$HostingServerName <- as.character(res$HostingServerName)

odbcClose(dbhandle)

## Data cleaning & Processing ##
res$VDI_Branch <- ifelse(grepl("AU TM", res$Name), "TM", "MW")
res$LoginId <- toupper(res$LoginId)
res$VDI_Name <- gsub("GLOBAL\\\\", "", res$LoginId)
res$VDI_Name <- factor(res$VDI_Name, levels=vdi_list_vector)

res$DateBin <- NA
res$DateBin[res$Date<="2017-12-15"] <- 'PREVIOUS_WEEK'
res$DateBin[res$Date >= "2017-12-18"] <- 'CURRENT_WEEK'

require(dplyr)
vdilogon <- res %>% group_by(Date, VDI_Name) %>% summarize(meanlogon=mean(logonduration)) %>% ungroup() %>% spread(VDI_Name, meanlogon, fill=0)
res %>% group_by(Date, VDI_Name) %>% summarize(meanlogon=mean(logonduration)) %>% ungroup() %>% spread(Date, meanlogon, fill=0)
vdiMW_logon <- tapply(res$logonduration, list(res$VDI_Name, res$Date), mean)
vdiMW_logon <- as.data.frame(vdiMW_logon)
vdiMW_logon$VDI_Branch <- "MW"

vdiTM_logon <- tapply(res$logonduration, list(res$VDI_Name, res$Date), mean)
vdiTM_logon <- as.data.frame(vdiTM_logon)
vdiTM_logon$VDI_Branch <- "TM"

vdi_logon <- tapply(res$logonduration, list(res$VDI_Name, res$DateBin), function(x) {mean(x, na.rm = T)})
vdi_logon <- as.data.frame(vdi_logon)
vdi_logon$VDI_Name <- row.names(vdi_logon)
vdiBOTH_logon <- merge(vdi_logon, vdi_list, by="VDI_Name")
vdiBOTH_logon$LogonTimeDiff <- vdiBOTH_logon$CURRENT - vdiBOTH_logon$PREVIOUS
#vdiBOTH_logon$LogonTimeDiff <- vdiBOTH_logon$`2017-12-19` - vdiBOTH_logon$`2017-12-15`
vdiBOTH_logon$LogonTimeStatus <- ifelse(vdiBOTH_logon$LogonTimeDiff >= 0, "Increase", "Decrease")
vdiBOTH_logon$LogonTimeStatus <- ifelse(vdiBOTH_logon$LogonTimeDiff == 0, "Same", vdiBOTH_logon$LogonTimeStatus)

# vdiBOTH_logon <- rbind(vdiTM_logon, vdiMW_logon)
# vdiBOTH_logon$VDI_Name <- rownames(vdiBOTH_logon)
# vdiBOTH_logon$difference <- vdiBOTH_logon$`2017-12-18` - vdiBOTH_logon$`2017-12-15`
# vdiBOTH_logon$LogonTimePerformance <- ifelse(vdiBOTH_logon$difference <= 0, "Increase", "Decrease")

write.csv(vdiBOTH_logon, file="datafiles/vdi_MW_TM_logon_2nd.csv")

slices <- table(vdiBOTH_logon$LogonTimeStatus[vdiBOTH_logon$VDI_Branch=='MW'], exclude = NULL)
pct <- round(slices/sum(slices)*100)
labels <- paste0(names(slices), ' - ',pct, "%")
pie(slices, labels, main="MW Logon Time Status After AV Enabled", col=c("limegreen", "salmon", "grey"))

slices <- table(vdiBOTH_logon$LogonTimeStatus[vdiBOTH_logon$VDI_Branch=='TM'], exclude = NULL)
pct <- round(slices/sum(slices)*100)
labels <- paste0(names(slices), ' - ',pct, "%")
pie(slices, labels, main="TM Logon Time Status After AV Enabled", col=c("limegreen", "salmon", "skyblue", "grey"))

###############################

####### Histogram Comparison of users #####
rm(list=ls())
setwd("//SVRSG001RPS01.asia.corp.anz.com/pradana1$/My Documents/SGWS-8467837A_notbackedup")
source('Rscript/sqlconn.R')
df <- sqlconn('2017-12-01', '2017-12-22')
df <- subset(df, logonduration > 0)
df$Week[df$Date <= '2017-12-07'] <- 'week1'
df$Week[df$Date >= '2017-12-08' & df$Date <= '2017-12-15'] <- 'week2'
df$Week[df$Date >= '2017-12-16'] <- 'week3'
breaks <- c(0,50,100,150,200,250,300,350,400,450)
hist(df$logonduration[df$Week=='previous'],breaks=8, xlim=c(0,400), main = '2017-12-01 to 2017-12-08', xlab = 'logonduration', col='skyblue')
hist(df$logonduration[df$Week=='current'],breaks=8, xlim=c(0,400), main = "2017-12-08 to 2017-12-15", xlab = 'logonduration', col='salmon')
table(cut(df$logonduration[df$Week=='previous'], breaks=breaks))
table(cut(df$logonduration[df$Week=='current'], breaks=breaks))

hist(df$logonduration,breaks=16, xlim=c(0,400), main = '2017-12-15 to 2017-12-22', xlab = 'logonduration', col='orangered')
table(cut(df$logonduration, breaks=breaks))

dfweek1 <- as.data.frame(table(cut(df$logonduration[df$Week=='week1'], breaks=breaks)))
names(dfweek1)[2] <- 'Weekof1stDec'
dfweek2 <- as.data.frame(table(cut(df$logonduration[df$Week=='week2'], breaks=breaks)))
names(dfweek2)[2] <- 'Weekof8thDec'
dfweek3 <- as.data.frame(table(cut(df$logonduration[df$Week=='week3'], breaks=breaks)))
names(dfweek3)[2] <- 'Weekof16thDec'

dfweekcompare <- cbind(dfweek1,dfweek2$Weekof8thDec,dfweek3[,2])
names(dfweekcompare) <- c('bins', 'Weekof1stDec', 'Weekof8thDec', 'Weekof16thDec')

dfweekcomparelong <- melt(dfweekcompare, id=c("bins"))
#chart
ggplot(data=dfweekcomparelong, aes(x=bins, y=value, fill=variable)) + 
  geom_bar(stat='identity', position='dodge') + 
  geom_text(aes(label=value), position = position_dodge(0.9), size=2.7, vjust=-0.7) +
  ggtitle("Frequency of logontime by Week")


###########################################
############ END VDI Analysis #############
###########################################



################ TEXT ANALYSIS ################
require(openxlsx)
feedback <- read.xlsx('UserFeedback - ToEdit.xlsx',rows=1:337)
feedback <- feedback[,5]
names(feedback)[1] <-  'text'
feedback <- na.omit(feedback)
matrix=create_matrix(fbSub[,1], 
                     language = 'english', 
                     removeNumbers = T,
                     removeStopwords = F,
                     stemWords = F)
mat = as.matrix(matrix)

classifier = naiveBayes(mat[1:2,], as.factor(fbSub[1:2,2]) )

predicted = predict(classifier, mat[41:61,]); predicted
table(fbSub[41:61, 2], predicted)
recall_accuracy(fbSub[41:61, 2], predicted)

data <- feedback[,1]

corpus <- VCorpus(VectorSource(data))
docs <- tm_map(corpus, content_transformer(tolower))
docs <- tm_map(corpus, removePunctuation)
docs <- tm_map(corpus, removeNumbers)
docs <- tm_map(corpus, removeWords, stopwords())
docs <- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(docs)
freq <- colSums(as.matrix(dtm))




### TEST1
data1 <- c('Bats','dogs','dogs')
corpus <- VCorpus(VectorSource(data))
tdm <- DocumentTermMatrix(corpus)

inspect(tdm)

##Create document-term matrix
tdm <- DocumentTermMatrix(corpus,
                          list(removePunctuation=T,
                               stopwords=T,
                               stemming=T,
                               tolower=T))


## N-gramming 2 gram
feedback <- as.data.frame(feedback)
fb_bigrams <- feedback %>%
  unnest_tokens(bigram, feedback, token = "ngrams", n = 2)

fb_bigrams

fb_bigrams %>% count_('bigram',sort=T)

library(tidyr)

fb_separated <- fb_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

fb_filtered <- fb_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
fb_counts <- fb_filtered %>% 
  count_(c('word1', 'word2'), sort = TRUE)
fb_counts

fb_united <- fb_filtered %>%
  unite(bigram, word1, word2, sep = " ")

fb_united

fb_united_counts <- fb_united %>% count_('bigram',sort=T)

#### VISUALIZATION OF WORDS ####

## Wordcloud
  
wordcloud(names(freq[1:30]), freq, colors =rainbow(30), random.color = F)

## Barplot
library(ggplot2)
g <- ggplot() + geom_bar(stat='identity',data=fb_united_counts[1:20,], 
                         aes(x=reorder(bigram,n),n,fill=n),width=0.9) +
     geom_text(data=fb_united_counts[1:20,], aes(label=n))
     coord_flip() +
     xlab('Phrases') + ylab('No. of Occurence') + ggtitle('N-gram Buckets') +

g <-  ggplot(data=fb_united_counts[1:20,], aes(x=reorder(bigram,n), y=n, fill=bigram)) + 
      geom_bar(stat='identity', show.legend = FALSE) + 
      coord_flip() +
      geom_text(aes(label=n), hjust=2 ,colour='white') +
      xlab('Phrase') + ylab('No. occurence') + ggtitle('N-gram Counting')
      #scale_fill_gradientn(colours=c('#61a8ff', '#4477b2'))
g

###############################################


######## Text Analysis on EasyTech #########

## 1. Counting words ##

## reading html data into text vector
require(rvest)
webpage <- read_html("C:/NotBackedUp/aditya/Making Technology Easy.html")
#webpage <- read_html("C:/NotBackedUp/aditya/Techstore_Comment.html")
comment_data_html <- html_nodes(webpage, '.ngSummary')
comment_data <- html_text(comment_data_html)

require(tidytext)
## counting words
## with username
comment_df <- data.frame(line=1:length(comment_data), comment=comment_data, username = user_name, stringsAsFactors = FALSE)
## without username
comment_df <- data.frame(line=1:length(comment_data), comment=comment_data, stringsAsFactors = FALSE)
comment_data_tokens <- comment_df %>% unnest_tokens(word, comment)

## removing stop words (the, a, be, etc.)
comment_data_clean <- comment_data_tokens %>% anti_join(stop_words) %>% count(word,sort=T)
comment_data_clean <- comment_data_clean %>% count(word, sort=T)

## counting sentiments
comment_sentiment <-  comment_data_tokens %>% 
                      inner_join(bing_sentiment2) %>% 
                      count(word, sentiment, sort=T) %>% 
                      spread(sentiment, n, fill=0) %>% 
                      mutate(sentiment = positive-negative) 

ggplot(comment_sentiment, aes(word, sentiment)) + geom_col(show.legend = F)

comment_sentiment_count <- comment_data_tokens %>% 
  inner_join(bing_sentiment2) %>% 
  count(word, sentiment, sort=TRUE) %>% 
  ungroup()

comment_sentiment_count %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% ungroup() %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n,fill=sentiment)) + geom_col() + facet_wrap(~sentiment, scales='free_y') + coord_flip()


## Scoring Sentiments per comments

## Comments --unnest_token--
comment_data_tokens <- comment_df_clean %>% unnest_tokens(word, comment)
## Words per Comments --count_positive_&_negative--
## filter easy and like
bing_sentiment2 <- get_sentiments("bing") %>% filter(!word %in% c('easy','like', 'agile'))
bing_sentiment2 <- bind_rows(data.frame(word=c('less'), sentiment=c('negative')), bing_sentiment2)
comment_data_sentiment <- comment_data_tokens %>% inner_join(bing_sentiment2)
## Add/Subtract Positive and negative words -> get score
df1 <- comment_data_sentiment
df1$value <- NA
df1$value <- ifelse(df1$sentiment == 'negative', -1, 1)
df1_score <- df1 %>% group_by(line) %>% summarise(score=sum(value))
df1_score$sentiment <- NA
df1_score$sentiment <- ifelse(df1_score$score>0, 'positive', 'negative')

df1_score %>% ggplot(aes(line, score, fill=sentiment))+geom_col()

## exclude our group user
user_name_html <- html_nodes(webpage, '.ngAuthorLink')
user_name_exclude_list <- grep('ngAuthorLikeLink', html_attrs(user_name_html))
user_name_html <- user_name_html[-user_name_exclude_list]
user_name <- html_text(user_name_html)
comment_df$username <- user_name
gts_username <- c('Krishnan, Manu', 'Chakraborty, Bikash', 'Adhia, Rushikesh', 'Patel, Pankaj')

comment_df_clean <- comment_df %>% filter(!username %in% gts_username)

## add words into sentiment list
bing_sentiment2 <- bind_rows(data.frame(word=c('less'), sentiment=c('negative')), bing_sentiment2)
############################################



######## READING Citrix Data #########
##### Calculating Heatmap for   ######
##### all countries & branches  ######
######################################
require(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};
                              server=SQLAU001MEL0220\\DWH;
                              uid=RUSER;PWD=P@ssw0rd;
                              database=DWH;')

#### Get all distinct branches
query <- paste("SELECT DISTINCT Name FROM Citrix_MW_TM_FACT",
               "WHERE Name not like '%PREPROD' and Name not like '%TEST' and Name not like 'TEMP'",
               "AND LogonStartDate > '2017-01-01' ",
               "ORDER BY Name")
print(query)

branches <- sqlQuery(dbhandle, query)
branches$Name <-  as.character(branches$Name)
branches$Category <- "3rd Party"
branches$Category <- ifelse(grepl('corporate', branches$Name, ignore.case = T), 
                            "Corporate", branches$Category)
branches$Category <- ifelse(grepl('au mw|au tm', branches$Name, ignore.case = T), 
                            "AU", branches$Category)

#### Get all distinct countries
query <- paste("SELECT DISTINCT LocationCountry FROM Citrix_MW_TM_FACT",
               "WHERE Name not like '%PREPROD' and Name not like '%TEST' and Name not like 'TEMP'",
               "AND LogonStartDate > '2017-01-01' ",
               "ORDER BY LocationCountry")
print(query)
countries <- sqlQuery(dbhandle, query)
countries$LocationCountry <-  as.character(countries$LocationCountry)


#### Get statistics of logonduration
date1 <- Sys.Date()-13
date2 <- Sys.Date()+1
conditionDate <- paste0("LogonStartDate >= '", date1, "' AND ", "LogonStartDate <= '", date2,"'")

# old query to immediately get mean
# query <- paste("SELECT LocationCountry, Name, AVG(logonduration) as Mean FROM Citrix_MW_TM_FACT",
#                "WHERE Name not like '%PREPROD' and Name not like '%TEST' and Name not like 'TEMP'",
#                "AND", conditionDate,
#                "AND", "logonduration > 0",
#                "GROUP BY LocationCountry, Name",
#                "ORDER BY LocationCOuntry, Name")

# new query to get all logonduration
query <- paste("SELECT LogonstartDate, LocationCountry, Name, logonduration FROM Citrix_MW_TM_FACT",
               "WHERE Name not like '%PREPROD' and Name not like '%TEST' and Name not like 'TEMP'",
               "AND", conditionDate,
               "AND", "logonduration > 0",
               #"GROUP BY LocationCountry, Name",
               "ORDER BY LogonstartDate")
print(query)

res <- sqlQuery(dbhandle, query)
odbcClose(dbhandle)

res$Name <- as.character(res$Name)
res$LocationCountry <- as.character(res$LocationCountry)
res$LogonstartDate <- as.Date(res$LogonstartDate, tz='Asia/Singapore')

#Adding Category Column
res <- merge(res,branches, by='Name')[,union(names(res),names(branches))]

## Divide dataframe into previous week and current week
res.prev <- subset(res, LogonstartDate >= max(res$LogonstartDate)-13 & LogonstartDate <= max(res$LogonstartDate)-7)
res.curr <- subset(res, LogonstartDate >= max(res$LogonstartDate)-6 & LogonstartDate <= max(res$LogonstartDate))

## Calculate mean, median prev for previous week
meanprev <- tapply(res.prev$logonduration, list(res.prev$Category,res.prev$LocationCountry), function(x) round(mean(x),digits=2)  )
medianprev <- tapply(res.prev$logonduration, list(res.prev$Category,res.prev$LocationCountry), median)
sdprev <- tapply(res.prev$logonduration, list(res.prev$Category,res.prev$LocationCountry), function(x) round(sd(x),digits=2))

## Calculate mean, median prev for current week
meancurr <- tapply(res.curr$logonduration, list(res.curr$Category,res.curr$LocationCountry), function(x) round(mean(x),digits=2)  )
mediancurr <- tapply(res.curr$logonduration, list(res.curr$Category,res.curr$LocationCountry), median)
sdcurr <- tapply(res.curr$logonduration, list(res.curr$Category,res.curr$LocationCountry), function(x) round(sd(x),digits=2))


## Cleaning data. Remove columns that have all NA
x <- colSums(meancurr, na.rm=T)==0
meancurr <- meancurr[,!x]
x <- colSums(mediancurr, na.rm=T)==0
mediancurr <- mediancurr[,!x]
x <- colSums(sdcurr, na.rm=T)==0
sdcurr <- sdcurr[,!x]



# res.prev.df <- as.data.frame(as.table(meanprev))
# medianprev <- as.data.frame(as.table(medianprev))
# sdprev <- as.data.frame(as.table(sdprev))
# 
# ## Renaming column and adding median + SD column and adding Category column
# names(res.prev.df) <- c("LocationCountry", "Name", "Mean")
# res.prev.df$LocationCountry <- as.character(res.prev.df$LocationCountry)
# res.prev.df$Name <- as.character(res.prev.df$Name)
# res.prev.df <- cbind(res.prev.df, Median=medianprev$Freq, SD=sdprev$Freq)
# 
# 
# res.prev.df <- res.prev.df[complete.cases(res.prev.df),]
# mean.prev.agg <- setNames(aggregate(res.prev.df$Mean, 
#                                     by=list(res.prev.df$LocationCountry, res.prev.df$Category), 
#                                     mean),
#                          c('LocationCountry', 'Category', 'Mean'))
# median.prev.agg <- setNames(aggregate(res.prev.df$Median, 
#                                     by=list(res.prev.df$LocationCountry, res.prev.df$Category), 
#                                     median),
#                           c('LocationCountry', 'Category', 'Median'))
# sd.prev.agg <- setNames(aggregate(res.prev.df$SD, 
#                                       by=list(res.prev.df$LocationCountry, res.prev.df$Category), 
#                                       sd),
#                             c('LocationCountry', 'Category', 'StdDev'))
# 
# mean.prev.agg$Mean <- round(mean.prev.agg$Mean, digits=2)
# 

## Creating matrix
#Mean
# uniqueCountry <- countries$LocationCountry
# uniqueCountry <- unique(mean.prev.agg$LocationCountry)
# uniqueCategory <- unique(branches$Category)
# mat <- matrix(NA, nrow = length(uniqueCategory), 
#               ncol = length(uniqueCountry), byrow=T,
#               dimnames = list(uniqueCategory, uniqueCountry))
# rm(uniqueCountry, uniqueCategory)
# for(i in 1:nrow(mean.prev.agg))
# {
#   mat[rownames(mat)==mean.prev.agg[i,'Category'],colnames(mat)==mean.prev.agg[i,'LocationCountry']] <- mean.prev.agg[i,'Mean']
# }
# #Median
# uniqueCountry <- countries$LocationCountry
# uniqueCountry <- unique(median.prev.agg$LocationCountry)
# uniqueCategory <- unique(branches$Category)
# mat <- matrix(NA, nrow = length(uniqueCategory), 
#               ncol = length(uniqueCountry), byrow=T,
#               dimnames = list(uniqueCategory, uniqueCountry))
# rm(uniqueCountry, uniqueCategory)
# for(i in 1:nrow(median.prev.agg))
# {
#   mat[rownames(mat)==median.prev.agg[i,'Category'],colnames(mat)==median.prev.agg[i,'LocationCountry']] <- median.prev.agg[i,'Median']
# }

#SD


#Heatmap
require(gplots)

matrixlist <- list(meanprev, medianprev, sdprev)
matrixlist <- list(meancurr, mediancurr, sdcurr)
statstring <- c('Mean','Median','SD')

for (i in 1:length(matrixlist))
{
  mat <- matrixlist[[i]]
  dev.new()
  heatmap.2(mat,
            dendrogram = 'none',
            trace='none',
            colsep=1:ncol(mat),
            rowsep=1:nrow(mat),
            sepwidth=c(0.001,0.001),
            sepcolor="white",
            margins = c(9,9), 
            cexRow =1.0, cexCol=1.0,
            main = paste("Heatmap", statstring[i] ,"Logon of Country vs Branch",
                         "\n", min(res.curr$LogonstartDate), "to", max(res.curr$LogonstartDate)),
            xlab = 'Country', ylab = 'Branch',
            col = colorRampPalette(c('green3','greenyellow','gold','tomato','tomato2'))(64),
            breaks = c(seq(from=45, to=200, length.out=65)),
            #col=(heat.colors(128)),
            na.color='grey',
            cellnote = mat,
            notecol = 'black',
            srtCol=45, srtRow=0, offsetRow = -0.5, offsetCol = -0.75,
            key = TRUE,
            density.info = 'none',
            lhei=c(1,3.4), lwid=c(1,4), keysize = 1.0, key.par = list(cex=0.6)
  )
  
}
dev.off()

##Draw for heatmap of current week
dev.new()
mat <- sdcurr
heatmap.2(mat,
          dendrogram = 'none',
          trace='none',
          colsep=1:ncol(mat),
          rowsep=1:nrow(mat),
          sepwidth=c(0.001,0.001),
          sepcolor="white",
          margins = c(10,10), 
          cexRow =1.0, cexCol=1.0,
          main = paste("Heatmap Mean Logon of Country vs Branch",
                        "\n", min(res.curr$LogonstartDate), "to", max(res.curr$LogonstartDate)),
          xlab = 'Country', ylab = 'Branch',
          col = colorRampPalette(c('green3','greenyellow','gold','tomato','tomato2'))(64),
          breaks = c(seq(from=45, to=200, length.out=65)),
          #col=(heat.colors(128)),
          na.color='grey',
          cellnote = mat,
          notecol = 'black',
          srtCol=45, srtRow=0, offsetRow = -0.5, offsetCol = -0.75,
          key = TRUE,
          density.info = 'none', na.rm=T,
          lhei=c(1,4), lwid=c(1,4), keysize = 0.75, key.par = list(cex=0.6)
          )
#dev.new()

######################################


######### Dynamic Column DataFrame ##########

rm(list=ls())
df = data.frame(A1=seq(1,10),A2=LETTERS[1:10])
idloc=c(5,8)
seq_id <- c()
prev_n = 
next_n = 1
for(j in 1:length(idloc))
{
  seq_x <- (idloc[j]-prev_n):(idloc[j]+next_n)
  seq_x <- ifelse(seq_x < 1, NA, seq_x)
  seq_id <- append(seq_id, seq_x )
}

eventseq <- rep(seq(0-prev_n,0+next_n,1),length(idloc))
cor_df <- df[seq_id,]
cor_df$Occurence <- rep(1:length(idloc),each=(prev_n+next_n)+1)
cor_df$EventSequence <- eventseq

collist <- paste(seq(0-prev_n,0+next_n,1), sep="")
output_df <- as.data.frame(matrix(rep(0, ncol=length(collist), nrow=5)))
output_df <- as.data.frame(matrix(data=NA, ncol=length(collist),nrow=length(unique(cor_df$Occurence))))
output_df
names(output_df) <- collist
names(output_df)[length(collist)]

for(j in 1:length(collist))
{
  output_df[j] <- cor_df[cor_df$EventSequence == as.numeric(collist[j]),]$A2
}


#############################################

##############################
for(i in 1:ncol(df113.edit))
{
  df113.edit[,i] <- as.factor(df113.edit[,i])
}

setof5 <- paste(df113$`-5`,df113$`-4`,df113$`-3`,df113$`-2`,df113$`-1`,collapse=",")
setof6 <- paste(df113$`-5`,df113$`-4`,df113$`-3`,df113$`-2`,df113$`-1`,df113$`0`,sep=",")
setof6cat <- paste(setof6, collapse="\n")
write(setof6cat, file="test_set6")
tr2 <- read.transactions("test_set6", format='basket', sep=",")

## Concatenate rows of dataframe into string
#apply(dataframe, 1, paste, collapse="")



###############################
clientID <- c(rep(44291,6))

transDate <- c('2016-06-09 22:10:47', '2016-06-09 22:11:47', '2016-06-10 10:16:00', '2016-06-10 10:20:37',
               '2016-06-10 10:55:00','2016-06-13 11:09:00')
ApprovDate <- c(rep('2016-06-16 18:21:44', 6))

df <- data.frame(clientID=clientID, transDate=transDate, approvDate = ApprovDate, stringsAsFactors = F)

df1 <- data.frame(x <- sample(LETTERS[1:10], 26, replace=T),
                  y <- c(rep(c('Low','Medium'),10), rep('High', 6))
                  )
                  

########## SENTIMENT Analysis ###########
########## LiveChatWithHamish ###########

require(rvest)
require(dplyr)
require(tidytext)
require(ggplot2)

fileloc <- "downloads/LiveChatWithHamish2.html"
webpage <- read_html(fileloc)

comment_data <- webpage %>% html_nodes(".ngSummary") %>% html_text()
user_name <- webpage %>% html_nodes("[class=ngAuthorLink]") %>% html_text()
class_name <- webpage %>% html_nodes(".ngSummary") %>% html_attr("class")
comment.df <- data.frame(line=1:length(comment_data),
                          classtype=class_name,
                          comment=comment_data,
                          comment_clean = comment_data,
                          username = user_name, 
                          stringsAsFactors = FALSE)

ngsummary_v <- which(comment.df$classtype == 'ngSummary')
comment.df$questionline <- NA
comment.df$questionid <- NA
i = 1
for(n in ngsummary_v)
{
  comment.df$questionline[n:length(comment.df$questionid)] <- n
  comment.df$questionid[n:length(comment.df$questionid)] <- i
  i <- i+1
}
rm(i, n)

easytech.df$comment_clean <- gsub(' rob ', '', 
                                  easytech.df$comment_clean,
                                  ignore.case = T
)

write.csv(comment.df, 'hamish_sentiment.csv')

## SENTIMENT Analysis

easytech.commentonly <- easytech.df %>% filter(classtype != 'ngSummary')
## method 1 by words

easytech.word <- easytech.commentonly %>% unnest_tokens(word, comment_clean)

## 2. Join easytech comment's words with sentiment word reference
easytech.sentiment <- easytech.word %>% inner_join(get_sentiments('nrc'))

## 3. Sentiment count
sentiment_count <- easytech.sentiment %>% count(sentiment)
# Visualize
ggplot(sentiment_count, aes(sentiment, n, fill=sentiment)) + geom_col()

## 4. Word Count
wordcount <- easytech.word %>% anti_join(stop_words) %>%
              count(word) %>% filter(n>3) %>% arrange(-n) %>% 
              mutate(word=reorder(word,n))
# Visualize
ggplot(wordcount, aes(word, n, fill=word)) + 
  geom_col() + 
  geom_text(aes(label=n), hjust=-0.3) + 
  coord_flip()

## 5. Wordcloud
require(wordcloud2)
wordcloud2(wordcount, size=0.8, ellipticity = 0.8)

#########################################


############ Churn Analysis #################
rm(list=ls())
## Set Home Dir
setwd("C:/NotBackedUp/downloads/Azure-ML-Churn-Tutorial-master/Azure-ML-Churn-Tutorial-master/")

## Read Data
df <- read.csv("Customer Churn Data.csv")

## Clean data
# UpperCase and Trimming Whitespace
df$Churn_ <- toupper(df$Churn_)
df$Int_l_Plan <- trimws(df$Int_l_Plan)
df$VMail_Plan <- trimws(df$VMail_Plan)

# dropping certain column
drops <- c('Day_Charge', 'Eve_Charge', 'Night_Charge', 'Intl_Charge', 
           'X_dataobs', 'Area_Code', 'Phone') 
df_binary <- df[, !(names(df) %in% drops)]

rm(drops)

## Splitting data into training & validation set
train_size <- 0.7
set.seed(1)
train_ind <- sample(seq_len(nrow(df_binary)), size = floor(train_size * nrow(df_binary)))
train_set <- df_binary[train_ind,]
test_set <- df_binary[-train_ind,]





########## End Churn Analysis ###############



#### DPLYR quick shortcut ####
library(dplyr)

## Count unique occurence
n_distinct(df$variable)
length(unique(df$variable))

## Count something by group
df %>% group_by(varA) %>% summarise(value = mean(varB))



###############################



## ============ TWITTER ================== ##
rm(list=ls())
require(twitteR)
require(httr)
require(base64enc)

#Sys.setenv(http_proxy="http://10.62.36.14:80", https_proxy="http://10.62.36.14:80")

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

api_key <- 'VyawrF9rwgNIwmzpdysAKJVXg'
api_secret <- 'OSRyPLxPspQ2QLxWh3DhyCa9vFbHUxrbQCNTH1l3YBvXAXJAMk'
token <- '47151243-iLQ930rd04pdWbMHGs41XauahJnySHdzGxm91NlJ7'
token_secret <- 'XMSdUcNUeXOEwqSjnuFfAUiLx8izMi9iFGzLNJx64Xn8k'

setup_twitter_oauth(api_key, api_secret, token, token_secret)


## File merging ##
library(openxlsx)
sourcefolder <- '\\\\10.111.240.124\\Packaging$\\Packages\\1.New\\PEGA\\MIS PEGA Data'
list.files(sourcefolder)

read



