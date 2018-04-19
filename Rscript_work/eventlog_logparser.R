#### Event Log Parser ####
## Input: EVTX file
## Output: CSV File

## function to check where the script is ran
thisFile <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    # 'source'd via R console
    return(normalizePath(sys.frames()[[1]]$ofile))
  }
}

logparser <- function(filename, folderloc)
{
  cat("######## START SCRIPT ########\n")
  ######## Determine where the script is run ########
  script.dir <- paste0(dirname(thisFile()),"/")
  cat(paste("\nScript Directory: ",script.dir))
  
  ###################################################
  
  ######## Check if Log Parser exists #########

  if(!file.exists("C:/Program Files (x86)/Log Parser 2.2/LogParser.exe"))
  {
    stoptext <- paste("\nLog Parser does not exist.",
                      "Install Log Parser (Log Parser 2.2/LogParser.exe).",
                      "\nQuitting Script")
    stop(stoptext)
  }
  
  #############################################
  
  ########################## Initialize #############################
  #setwd("C:/NotBackedUp")
  #getwd()
  .libPaths('C:/NotBackedUp/R/win-library/3.4.1')
  
  ###################################################################
  
  
  
  ################## Validating if folder/file exists ####################
  
  ## Validate user is fiving filename parameter
  
  if(missing(filename) || !is.character(filename))
  {
    #stop("No Filename given")
    cat("\nFilename is wrong or not given properly. Revert to default filename")
    filename <- "MyApplication.evtx"
  } else {
    cat(paste("\n\tfilename is",filename))
  }
  
  fileloc <- ""  
  ## Validate user is giving folderloc parameter
  if(missing('folderloc') || !is.character(folderloc))
  {
    cat("\nNo folder location given")
    
    ## find in current script directory & datafiles directory
    fileloc1 <- paste0(script.dir,filename)
    fileloc2 <- paste0(dirname(script.dir),"/datafiles/",filename)
    
    ## Validate file existence
    if(!file.exists(fileloc1) && !file.exists(fileloc2) )
    {
      cat(paste0("\nFile does not exist in current script directory or '../datafiles/'.",
                  "\nPlease give proper folder location of the your file."))
      #stop()
    } else if(file.exists(fileloc1)) {
      
      fileloc <-  fileloc1
      
    } else if(file.exists(fileloc2)) {
      
      fileloc <-  fileloc2
      
    }
    rm(fileloc1,fileloc2)
  } else {
    ## Validate foldername
    if(!(endsWith(folderloc,'/') || endsWith(folderloc, '\\') ) )
    {
      folderloc <-  paste0(folderloc,'/')
    }
    cat(paste("\n\tFolder name is:", folderloc))
    
    ## Validate folder existence
    if(!dir.exists(folderloc))
    {
      stop("Folder does not exist")
    }
  
    ## Validate file existence
    fileloc <- paste0(folderloc,filename)
    if(!file.exists(fileloc))
    {
      stop("File does not exist")
    }
    
  }
  cat("\nFile Validation completes")
  cat(paste("\nFile Location: ", fileloc))
  
  
  ###################################################################
  
  
  
  ################## Parse log using log parser ####################
  
  #folderloc <- "\\NotBackedUp\\datafiles\\eventlogdata"
  #filename <- "\\MyApplication.evtx"
  filesource <- fileloc
  fileresult <- paste0(substr(fileloc, 1, nchar(filesource)-4), "csv")
  
  cmdstring <- paste0("logparser ",
                      "\"SELECT * INTO \'", fileresult,
                      "\' FROM \'", filesource, "\'\" ",
                      "-i:EVT -o:CSV")
  cat(paste0('\n',cmdstring,'\n'))
  
  shell(cmdstring)
  
  ## Clearing variables
  rm(cmdstring, filename, fileresult, filesource, folderloc)
  
  cat("######## END SCRIPT ########")
  ##################################################################
}