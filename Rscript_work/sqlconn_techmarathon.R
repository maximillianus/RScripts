#### Inject data to sQL for Tech marathon ####

############ Creating ODBC Connection ###########
rm(list=ls())
print("Creating ODBC Connection...")
require(RODBC)
# dbhandle <- odbcDriverConnect('driver={SQL Server};
#                               server=SQLAU001MEL0220\\DWH;
#                               uid=RUSER;PWD=P@ssw0rd;
#                               database=QuizDb;')

dbhandle <- odbcDriverConnect('driver={SQL Server};
                                server=SQLSG001SIN0047\\SQLHD02;
                              uid=RUSER;PWD=P@ssw0rd;
                              database=TechMarathon_test;')

#################################################


############ Read CSV data ############
setwd("\\\\SVRSG001RPS01.asia.corp.anz.com\\pradana1$\\My Documents\\SGWS-8467837A_notbackedup\\datafiles\\marathonquestion")

#df <- read.csv("marathon_qn_python_R_uploadDB_all.csv", stringsAsFactors = F)
df <- read.csv("marathon_qn_dummy_uploadDB_all.csv", stringsAsFactors = F)


#######################################


#### INSERTION SEQUENCE ####
sqlClear(channel = dbhandle,
         sqtable = 'dbo.DemoQuestionsTable',
         errors=TRUE)
sqlSave(channel = dbhandle,
        dat = (df),
        tablename = 'dbo.DemoQuestionsTable',
        rownames=F,
        append=T,
        verbose=F
)
#############################

#### DROP TABLE ####

## SQLAU001MEL0220 Database
# sqlDrop(channel = dbhandle,
#         sqtable = 'dbo.AdityaQuestionsTable',
#         errors=TRUE)
# 
# ## SQLSG001SIN0047 Database
# sqlDrop(channel = dbhandle,
#         sqtable = 'dbo.DemoQuestionsTable',
#         errors=TRUE)

####################

#### TRUNCATE TABLE ####

## SQLSG001SIN0047 Database
# sqlClear(channel = dbhandle,
#         sqtable = 'dbo.DemoQuestionsTable',
#         errors=TRUE)

####################

#### CREATE AND INSERT NEW TABLE ####

## SQLAU001MEL0220 Database
# sqlSave(channel = dbhandle,
#         dat = df,
#         tablename = 'dbo.AdityaQuestionsTable',
#         rownames=F,
#         verbose=T
# )
# 
# ## SQLSG001SIN0047 Database
# sqlSave(channel = dbhandle,
#         dat = df,
#         tablename = 'dbo.DemoQuestionsTable',
#         rownames=F,
#         verbose=F
# )
#####################################

#### APPEND TO EXISTING TABLE ####
# sqlSave(channel = dbhandle,
#         dat = (df),
#         tablename = 'dbo.AdityaQuestionsTable',
#         rownames=F,
#         append=T,
#         verbose=T
# )
# 
# sqlSave(channel = dbhandle,
#         dat = (df),
#         tablename = 'dbo.DemoQuestionsTable',
#         rownames=F,
#         append=T,
#         verbose=F
# )
##################################

#### Close ODBC Connection ####
print("Closing ODBC Connection")
odbcClose(dbhandle)

###############################

print("#### END SCRIPT ####")