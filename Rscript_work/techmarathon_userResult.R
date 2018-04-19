#### TechMarathon Calculate User Results ####

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


userid = 'liawc'
querystring1 <- paste0("SELECT * FROM dbo.TestResult WHERE UserID='", userid,"'")
querystring2 <- "SELECT QuestionID, Questions, CorrectAnswerID FROM dbo.DemoQuestionsTable
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

incorrect_df <- df[df$Correct==0,]
incorrect_df <- subset(incorrect_df, select = c("UserID", "Questions"))

#######################################


############ Get Result ############
print(incorrect_df)

####################################

############ Store Result ############
setwd("\\\\SVRSG001RPS01.asia.corp.anz.com\\pradana1$\\My Documents\\SGWS-8467837A_notbackedup\\datafiles\\marathonquestion")
filename = paste0("userResult/", userid, "_result.csv")
write.csv(x=incorrect_df, file=filename)


######################################


## Close connection
odbcClose(dbhandle)
rm(list=ls())

####################################

print("#### END Calculate User Results SCRIPT ####")
