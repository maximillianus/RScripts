#### Quiz Algorithm ####
#### As a start, gather 10 questions ####

#### Revisions #####
# 23-10-17: 
#   -Add snippet for categories with less than 5 difficulty levels
#   -Add conditional IF to skip loop for categories which has no questions        
#
####################


quizalgo <- function()
{
  ## Gathering user choices
  userchoices <- c("Python", "Windows", "R")
  userchoicesql <- paste0("('", paste(userchoices,collapse="','"), "')")
  
  ######## Making SQL Connection #########
  # SQL Handle
  require(RODBC)
  # dbhandle <- odbcDriverConnect('driver={SQL Server};
  #                               server=SQLAU001MEL0220\\DWH;
  #                               uid=RUSER;PWD=P@ssw0rd;
  #                               database=QuizDB;')
  
  dbhandle <- odbcDriverConnect('driver={SQL Server};
                                server=SQLSG001SIN0047\\SQLHD02;
                                uid=RUSER;PWD=P@ssw0rd;
                                database=TechMarathon_test;')
  
  # SQL Condition
  column <- "*"
  condition <- paste("SubCategory IN", userchoicesql)
  orderCondition <- "SubCategory"
  
  # SQL Query
  # query <- paste("SELECT DISTINCT", column,
  #                "FROM AdityaQuestionsTable",
  #                "WHERE", condition,
  #                "ORDER BY", orderCondition
  #               )
  
  query <- paste("SELECT DISTINCT", column,
                 "FROM QuestionsTable",
                 "WHERE", condition,
                 "ORDER BY", orderCondition
  )
  
  print(query)
  
  # Getting results table
  res <- sqlQuery(dbhandle, query, as.is=TRUE)
  #res$Weightage <- as.factor(res$Weightage)
  res$Weightage <- factor(res$Weightage, levels=c(1,2,3,4,5), labels=c(1,2,3,4,5))
  print(str(res))
  odbcClose(dbhandle)
  
  ##########################################
  
  ######## Random Algorithm #########
  require(fifer) #needed its stratify function
  for(i in 1:10)
  {
    #randomize dist of questions
    questiondist <- sample(c(3,3,4))
  }
  
  # Set weightage distribution for difficulty 1-to-5
  weightdist <- c(0.3,0.3,0.28,0.09,0.09)
  
  questions_df = data.frame()
  for(i in 1:3)
  {
    # Subset per SubCategory
    resCat <- subset(res, SubCategory == userchoices[i])
    if(nrow(df) != 0)
    {
      # Sampling questions weightage distribution
      samplingdist <- sample(resCat$Weightage,
                             questiondist[i], 
                             prob = rep(weightdist, table(resCat$Weightage))
                            )
      samplingdist <- sort(samplingdist)
      
      # Adding this code for questions that does not have all 5 difficulty levels
      samplinglevel <- sort(unique(resCat$Weightage))
      samplingdistf <- factor(samplingdist,
                              levels=samplinglevel,
                              labels=samplinglevel)
      
      # Sampling question based on weightage
      questions <- stratified(df=resCat, group = 'Weightage', size=table(samplingdistf))
      
      # Bind rows to form a single df
      questions_df <- rbind(questions_df, questions)
    }
    
  }
  
  
  ###################################
    
  # Printout Result
  print(questions_df)
  
  odbcClose(dbhandle)
}

quizalgo()
