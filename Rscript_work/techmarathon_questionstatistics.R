#### TechMarathon Question Statistics ####

print("#### START Question Statistics SCRIPT ####")
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
# querystring2 <- "SELECT QuestionID, CorrectAnswerID FROM dbo.DemoQuestionsTable
#                   ORDER BY QuestionID"

testresult_df <- sqlQuery(channel = dbhandle, query = querystring1)
names(testresult_df) <- c('ResultID', 'QuestionID', 'Answer', 'UserID', 'TimeTaken',
                          'Comments', 'Createdate')

#questionAnsRef_df <- sqlQuery(channel = dbhandle, query = querystring2)


#######################################


############ Question Stats ############

maxtime <- tapply(testresult_df$TimeTaken, testresult_df$QuestionID, max)
mintime <- tapply(testresult_df$TimeTaken, testresult_df$QuestionID, min)
meantime <- tapply(testresult_df$TimeTaken, testresult_df$QuestionID, mean)
sdtime <- tapply(testresult_df$TimeTaken, testresult_df$QuestionID, sd)
qncount <- tapply(testresult_df$TimeTaken, testresult_df$QuestionID, length)

########################################

### Create into df
questionStat_df <- data.frame(questionid=sort(unique(testresult_df$QuestionID)),
                              max=maxtime,
                              min=mintime,
                              mean=meantime,
                              sd=sdtime,
                              attempt=qncount)

questionStat_df <- questionStat_df[order(questionStat_df$questionid),]

############ Print Result ############
print(questionStat_df)
########################################

############ Store Result ############

## IF table exists, drop
if("QuestionStatisticTable" %in% sqlTables(dbhandle)$TABLE_NAME)
{
  print("Table exists")
  sqlDrop(channel = dbhandle,
          sqtable = 'dbo.QuestionStatisticTable',
          errors=TRUE)
  print("Table dropped")

  print("Storing Results...")
  ## Store results
  sqlSave(channel = dbhandle,
          dat = questionStat_df,
          tablename = 'dbo.QuestionStatisticTable',
          rownames=F,
          verbose=F
  )
  print("Results stored!")

} else {
  print("Table does not exist. Create table and store result...")
  ## Store results
  sqlSave(channel = dbhandle,
          dat = questionStat_df,
          tablename = 'dbo.QuestionStatisticTable',
          rownames=F,
          verbose=F
  )
  print("Results stored!")
}

######################################

## Close connection

odbcClose(dbhandle)
rm(list=ls())
##########################################