#### TechMarathon Calculate Results ####

print("#### START Calculate Results SCRIPT ####")
######## Creating ODBC Connection ########
rm(list=ls())
require(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};
                              server=SQLSG001SIN0047\\SQLHD02;
                              uid=RUSER;PWD=P@ssw0rd;
                              database=TechMarathon_test;')


##########################################

############ Read SQL data ############
#setwd("\\\\SVRSG001RPS01.asia.corp.anz.com\\pradana1$\\My Documents\\SGWS-8467837A_notbackedup\\datafiles\\marathonquestion")


querystring1 <- "SELECT * FROM dbo.TestResult"
querystring2 <- "SELECT QuestionID, CorrectAnswerID FROM dbo.DemoQuestionsTable
                  ORDER BY QuestionID"

testresult_df <- sqlQuery(channel = dbhandle, query = querystring1)
names(testresult_df) <- c('ResultID', 'QuestionID', 'Answer', 'UserID', 'TimeTaken',
                              'Comments', 'Createdate')

questionAnsRef_df <- sqlQuery(channel = dbhandle, query = querystring2)


#######################################


######### Calculate Correct Result #########
testresult_df$Correct <- NULL
df <- merge(testresult_df, questionAnsRef_df, by="QuestionID")
df$Correct <- ifelse(df$Answer == df$CorrectAnswerID, 1, 0)


## Group timing
df$TimeTaken <- as.character(df$TimeTaken)
timing <- strsplit(df$TimeTaken, split=" ")
df$TimeTaken <- sapply(timing,'[[',1)
df$TimeTaken <- as.double(df$TimeTaken)

totaltime <- tapply(df$TimeTaken, df$UserID, sum)
totaltime <- as.data.frame(totaltime)
totaltime$userID <- rownames(totaltime)

## Group score
score <- tapply(df$Correct, df$UserID, sum)
score <- as.data.frame(score)
score$userID <- rownames(score)


## Final Result
finalresult_df <- merge(score, totaltime, by="userID")
finalresult_df <- finalresult_df[order(-finalresult_df$score, finalresult_df$totaltime),]
finalresult_df$min <- finalresult_df$totaltime / 60
finalresult_df$min <- format(finalresult_df$min, digits=4)
names(finalresult_df) <- c("UserID", "Score", "TotalTimeSecs", "TotalTimeMins")

############ Get Result ############
print(finalresult_df)

####################################

############ Store Result ############

## IF table exists, drop
if("FinalResultsTable" %in% sqlTables(dbhandle)$TABLE_NAME)
{
  print("Table exists")
  sqlDrop(channel = dbhandle,
          sqtable = 'dbo.FinalResultsTable',
          errors=TRUE)
  print("Table dropped")
  
  print("Storing Results...")
  ## Store results
  sqlSave(channel = dbhandle,
          dat = finalresult_df,
          tablename = 'dbo.FinalResultsTable',
          rownames=F,
          verbose=F
  )
  print("Results stored!")
  
} else {
  print("Table does not exist. Create table and store result...")
  ## Store results
  sqlSave(channel = dbhandle,
          dat = finalresult_df,
          tablename = 'dbo.FinalResultsTable',
          rownames=F,
          verbose=F
  )
  print("Results stored!")
}




######################################


## Close connection
odbcClose(dbhandle)
rm(list=ls())

####################################

print("#### END Calculate Results SCRIPT ####")
