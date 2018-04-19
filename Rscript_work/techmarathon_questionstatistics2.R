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
query1 <- "SELECT QuestionID, CorrectAnswerID, Weightage FROM DemoQuestionsTable"

df <- sqlQuery(channel = dbhandle, query = query1)

#######################################


############ Creating Table ############
df$mean <- 0
df$mean <- ifelse(df$Weightage == 1, 30, df$mean)
df$mean <- ifelse(df$Weightage == 2, 45, df$mean)
df$mean <- ifelse(df$Weightage == 3, 60, df$mean)
df$mean <- ifelse(df$Weightage == 4, 75, df$mean)
df$mean <- ifelse(df$Weightage == 5, 90, df$mean)

df$max <- df$mean + 15
#df$min <- df$mean - 30
#df$halfmax <- df$mean + 15
#df$halfmin <- df$mean - 15

#########################################




############ Store Result ############

## IF table exists, drop
if("QuestionStatisticTable2" %in% sqlTables(dbhandle)$TABLE_NAME)
{
  print("Table exists")
  sqlDrop(channel = dbhandle,
          sqtable = 'dbo.QuestionStatisticTable2',
          errors=TRUE)
  print("Table dropped")
  
  print("Storing Results...")
  ## Store results
  sqlSave(channel = dbhandle,
          dat = df,
          tablename = 'dbo.QuestionStatisticTable2',
          rownames=F,
          verbose=F
  )
  print("Results stored!")
  
} else {
  print("Table does not exist. Create table and store result...")
  ## Store results
  sqlSave(channel = dbhandle,
          dat = df,
          tablename = 'dbo.QuestionStatisticTable2',
          rownames=F,
          verbose=F
  )
  print("Results stored!")
}

odbcClose(dbhandle)
rm(list=ls())





###########################################
print("#### END Question Statistics 2 SCRIPT ####")