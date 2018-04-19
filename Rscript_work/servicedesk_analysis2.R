#### Service Desk Analysis ####

#### Initialize ####
rm(list=ls())
setwd('C:/NotBackedUp/')



#### Read Data ####
library(openxlsx)
library(stringr)

filename <- "profile_issues.xlsx"
filepath <- paste0("datafiles/", filename)

df_full <- read.xlsx(filepath, detectDates = T)
df_full$Open.Date <- as.POSIXct("1899-12-30", tz="UTC") + as.difftime(df$Open.Date, units="days")
df_full$Last.Modified <- as.POSIXct("1899-12-30", tz="UTC") + as.difftime(df$Last.Modified, units="days")

df <- df_full

#### Data engineering ####

## Split Description column into user information & issue description

#df$Desc1 <- gsub('Issue Description.*','',df[,6])
df$ActionTaken <- gsub('.*Action Taken / TS', 'Action Taken / TS', df[,6])


df$LanID <- str_match(df$Description, regex("Lan ID.*:(.*?)\n", ignore_case = T))[,2]
df$LanID <- trimws(df$LanID)

df$SalaryID <- str_match(df$Description, "Salary ID.*:(.*?)\n")[,2]
df$SalaryID <- trimws(df$SalaryID)

df$Contact <- str_match(df$Description, "Contact Number:(.*?)\n")[,2]
df$Contact <- trimws(df$Contact)

df$WorkstationID <- str_match(df$Description, "Workstation ID.*:(.*?)\n")[,2]
df$WorkstationID <- trimws(df$WorkstationID)

df$AppName <- str_match(df$Description, "Application Name.*:(.*?)\n")[,2]
df$AppName <- trimws(df$AppName)

df$IssueDesc <- str_match(df$Description, "Issue Description.*:(.*?)\n")[,2]
# remove punctuation
df$IssueDesc <- str_replace_all(df$IssueDesc, '[[:punct:]]', '')
# lower case
df$IssueDesc <- tolower(df$IssueDesc)
# trim whitespace
df$IssueDesc <- trimws(df$IssueDesc)
df$IssueDescClassify <- NA
df$IssueDescClassify <- ifelse(grepl('unable to log', df$IssueDesc), 'unable to login', df$IssueDescClassify)
df$IssueDescClassify <- ifelse(grepl('unable to (see|access)', df$IssueDesc), 'accessibility issue', df$IssueDescClassify)
df$IssueDescClassify <- ifelse(grepl('unable to print', df$IssueDesc), 'unable to print', df$IssueDescClassify)
df$IssueDescClassify <- ifelse(grepl('(temp.*profile|profile.*temp)', df$IssueDesc), 'temp profile issue', df$IssueDescClassify)
df$IssueDescClassify <- ifelse(grepl('(temp.*log|log.*temp)', df$IssueDesc), 'temp profile issue', df$IssueDescClassify)
df$IssueDescClassify <- ifelse(grepl('(roam.*profile|profile.*roam)', df$IssueDesc), 'roam profile issue', df$IssueDescClassify)
df$IssueDescClassify <- ifelse(grepl('(load.*profile|profile.*load)', df$IssueDesc), 'profile loading issue', df$IssueDescClassify)
df$IssueDescClassify <- ifelse(is.na(df$IssueDescClassify) & grepl('profile', df$IssueDesc), 'other profile issues', df$IssueDescClassify )

df$IssueDescClassify <- ifelse(grepl('(profile.*(space|storage)|(space|storage).*profile)', df$IssueDesc), 'profile storage issue', df$IssueDescClassify)
df$IssueDescClassify <- ifelse(grepl('missing', df$IssueDesc), 'missing item or menu', df$IssueDescClassify)

df$IssueDescClassify <- ifelse(is.na(df$IssueDescClassify) & grepl('policy', df$IssueDesc), 'group policy issues', df$IssueDescClassify )
df$IssueDescClassify <- ifelse(is.na(df$IssueDescClassify) & grepl('slow|stuck|freeze', df$IssueDesc), 'performance issues', df$IssueDescClassify )
df$IssueDescClassify <- ifelse(is.na(df$IssueDescClassify) & grepl('log', df$IssueDesc), 'unable to login', df$IssueDescClassify )
df$IssueDescClassify <- ifelse(is.na(df$IssueDescClassify) & grepl('file', df$IssueDesc), 'files related issues', df$IssueDescClassify )
df$IssueDescClassify <- ifelse(is.na(df$IssueDescClassify) & grepl('blank|screen|display', df$IssueDesc), 'display issues', df$IssueDescClassify )
df$IssueDescClassify <- ifelse(is.na(df$IssueDescClassify) & grepl('launch|open|access', df$IssueDesc), 'app/item launching/opening issues', df$IssueDescClassify )


df$IssueDescClassify <- ifelse(is.na(df$IssueDescClassify), 'other issues', df$IssueDescClassify )
df$IssueDescClassify <- ifelse(is.na(df$IssueDescClassify) & (grepl('^$', df$IssueDesc) | is.na(df$IssueDesc)), 'No Information', df$IssueDescClassify )
View(df[is.na(df$IssueDescClassify),c("IssueDesc", "IssueDescClassify")])

remaining_words <- paste(df[is.na(df$IssueDescClassify),]$IssueDesc, collapse = ' ')

# Issue Desc classification
# df$IssueDescClassify <- str_match(df$Summary, regex(" - (.*?)", ignore_case = T))
# df$IssueDescClassify <- str_replace(df$IssueDescClassify, regex(".*profile.*load.*temp.*", ignore_case = T), "profile loads as temp")
# df$IssueDescClassify <- str_replace(df$IssueDescClassify, regex(".*temp.*profile.*", ignore_case = T), "profile loads as temp")
# df$IssueDescClassify <- str_replace(df$IssueDescClassify, regex(".*profile.*not.*load.*", ignore_case = T), "profile not loading")
# df$IssueDescClassify <- str_replace(df$IssueDescClassify, regex(".*unable.*log.*", ignore_case = T), "unable to login")
# 
# sort(table(df$IssueDescClassify), decreasing = T)[1:30]

df$IssueResolved <- str_match(df$Description, "Issue Resolved.*:(.*?)\n")[,2]
df$IssueResolved <- trimws(df$IssueResolved)
df$IssueResolved <- gsub(".*N.*", "N", df$IssueResolved, ignore.case = T)
df$IssueResolved <- gsub(".*Y.*", "Y", df$IssueResolved, ignore.case = T)

df$affectedUsers <- str_match(df$Description, "Number of users affected.*:(.*?)\n")[,2]
df$affectedUsers <- trimws(df$affectedUsers)

df$affectedUsers <- str_match(df$Description, "Number of users affected.*:(.*?)\n")[,2]
df$affectedUsers <- trimws(df$affectedUsers)

df$affectedUsers <- str_match(df$Description, "Number of users affected.*:(.*?)\n")[,2]
df$affectedUsers <- trimws(df$affectedUsers)

df$Incident.Area <- str_match(df$Incident.Area, ".W.(.*?).Function")[,2]
df$Incident.Area <- trimws(df$Incident.Area)


write.csv(df, file='datafiles/result_profile_issues.csv')

