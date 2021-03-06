#### TechMarathon Scoring System 2 ####
#### Statistic with pre-defined standards ####

print("#### START Scoring System SCRIPT ####")
##############################################

## Extract results from dbo.TestResults
######## Creating ODBC Connection ########
rm(list=ls())
require(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};
                              server=SQLSG001SIN0047\\SQLHD02;
                              uid=RUSER;PWD=P@ssw0rd;
                              database=TechMarathon_test;')


##########################################

## Join the QuestionID with QuestionStatistics

############ Read SQL data ############


userid = '%'
columns = paste('ResultID', 'QID','UserID', 'Answer','dbo.QuestionStatisticTable2.CorrectAnswerID',
                'TimeTaken','Createdate','dbo.QuestionStatisticTable2.max',
                'dbo.QuestionStatisticTable2.mean', 
                'dbo.QuestionStatisticTable2.Weightage', sep=", ")
querystring1 <- paste0("SELECT ",
                       columns,
                       " FROM dbo.TestResult ",
                       " LEFT JOIN dbo.QuestionStatisticTable2 ON TestResult.QID=QuestionStatisticTable2.QuestionID ",
                       #"LEFT JOIN dbo.DemoQuestionsTable ON TestResult.QID=DemoQuestionsTable.QuestionID ",
                       "WHERE UserID like'", userid,"'")

sql_df <- sqlQuery(channel = dbhandle, query = querystring1)



#######################################


############# Scoring Algorithm ###########
# within standard deviation: normal. 
# below lower 95% confidence interval: half score
#require(dplyr)

#sql_df$lowerconfInt <- sql_df$mean - sql_df$sd
sql_df$Correct <- NULL
sql_df$Correct <- ifelse(sql_df$Answer == sql_df$CorrectAnswerID, 1, 0)

sql_df$Score <- 0

# ## Score 5 if Answer is Correct and TimeTaken beyond max/min
# sql_df$Score <- ifelse(sql_df$Correct == 1 & (sql_df$TimeTaken > sql_df$max | sql_df$TimeTaken < sql_df$min), 5, sql_df$Score)
# 
# ## Score 7.5 if Answer is Correct and TimeTaken between max/min
# sql_df$Score <- ifelse(sql_df$Correct == 1 & (sql_df$TimeTaken < sql_df$max & sql_df$TimeTaken > sql_df$min), 7.5, sql_df$Score)
# 
# ## Score 10 if Answer is Correct and TimeTaken between halfmax/halfmin
# sql_df$Score <- ifelse(sql_df$Correct == 1 & (sql_df$TimeTaken < sql_df$halfmax & sql_df$TimeTaken > sql_df$halfmin), 10, sql_df$Score)

## Score 5 if Answer is Correct and TimeTaken beyond max/min
sql_df$Score <- ifelse(sql_df$Correct == 1 & (sql_df$TimeTaken < sql_df$mean) , 5 + 2.5, sql_df$Score)

sql_df$Score <- ifelse(sql_df$Correct == 1 & (sql_df$TimeTaken > sql_df$mean & sql_df$TimeTaken < sql_df$max) , 5 + 5, sql_df$Score)

sql_df$Score <- ifelse(sql_df$Correct == 1 & (sql_df$TimeTaken > sql_df$max) , 5 * 1.0, sql_df$Score)

print(sql_df)


#############################################


## Grouping correct answers, timing, and score for each users
## Group timing
sql_df$TimeTaken <- as.character(sql_df$TimeTaken)
timing <- strsplit(sql_df$TimeTaken, split=" ")
sql_df$TimeTaken <- sapply(timing,'[[',1)
sql_df$TimeTaken <- as.double(sql_df$TimeTaken)

totaltime <- tapply(sql_df$TimeTaken, sql_df$UserID, sum)
totaltime <- as.data.frame(totaltime)
totaltime$userID <- rownames(totaltime)

## Group total correct answers
totalcorrect <- tapply(sql_df$Correct, sql_df$UserID, sum)
totalcorrect <- as.data.frame(totalcorrect)
totalcorrect$userID <- rownames(totalcorrect)

## Group score
totalscore <- tapply(sql_df$Score, sql_df$UserID, sum)
totalscore <- as.data.frame(totalscore)
totalscore$userID <- rownames(totalscore)

## Final Result
finalresult_df <- merge(totalcorrect, totaltime, by="userID")
finalresult_df$min <- finalresult_df$totaltime / 60
finalresult_df$min <- format(finalresult_df$min, digits=4)
finalresult_df <- merge(finalresult_df, totalscore, by="userID")
finalresult_df <- finalresult_df[order(-finalresult_df$totalscore, finalresult_df$totaltime),]
names(finalresult_df) <- c("UserID", "TotalCorrect", "TotalTimeSecs", "TotalTimeMins", "TotalScore")

print(finalresult_df)

############ Store Result ############

## Store Summary Result
## IF table exists, drop
if("TempFinalResultsTable4" %in% sqlTables(dbhandle)$TABLE_NAME)
{
  print("Table exists")
  sqlDrop(channel = dbhandle,
          sqtable = 'dbo.TempFinalResultsTable4',
          errors=TRUE)
  print("Table dropped")

  print("Storing Results...")
  ## Store results
  sqlSave(channel = dbhandle,
          dat = finalresult_df,
          tablename = 'dbo.TempFinalResultsTable4',
          rownames=F,
          verbose=F
  )
  print("Results stored!")

} else {
  print("Table does not exist. Create table and store result...")
  ## Store results
  sqlSave(channel = dbhandle,
          dat = finalresult_df,
          tablename = 'dbo.TempFinalResultsTable4',
          rownames=F,
          verbose=F
  )
  print("Results stored!")
}

# ## Store Detailed Result
sql_df$UserID <- as.character(sql_df$UserID)
sql_df$Createdate <- as.character(sql_df$Createdate)
## IF table exists, drop
if("DetailedFinalResultsTable4" %in% sqlTables(dbhandle)$TABLE_NAME)
{
  print("Table exists")
  sqlDrop(channel = dbhandle,
          sqtable = 'dbo.DetailedFinalResultsTable4',
          errors=TRUE)
  print("Table dropped")

  print("Storing Detailed Results...")
  ## Store results
  sqlSave(channel = dbhandle,
          dat = sql_df,
          tablename = 'dbo.DetailedFinalResultsTable4',
          rownames=F,
          verbose=F
  )
  print("Detailed Results stored!")

} else {
  print("Table does not exist. Create table and store result...")
  ## Store results
  sqlSave(channel = dbhandle,
          dat = sql_df,
          tablename = 'dbo.DetailedFinalResultsTable4',
          rownames=F,
          verbose=F
  )
  print("Detailed Results stored!")
}


#############################################

odbcClose(dbhandle)
rm(list=ls())

############# PRINT Result #################


############################################


##############################################
print("#### END Scoring System SCRIPT ####")
