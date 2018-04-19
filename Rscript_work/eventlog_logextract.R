#### Event Log Extractor ####
## This is a script to extract     ##
## event log from user's computer. ##

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

eventextract <- function(logname)
{
  
  cat("######## START SCRIPT ########\n")
  
  ## Check script directory
  script.dir <- paste0(dirname(thisFile()),"/")
  cat(paste("\nScript Directory: ",script.dir),'\n')
  
  ## Create datafiles directory if it doesn't exist
  datafiles_path <- paste0(dirname(script.dir),'/datafiles/')
  if(!dir.exists(datafiles_path))
  {
    dir.create(datafiles_path)
    cat("\ndatafiles folder created!")
  }
  
  ## validating user input
  ## If no log specified, by default is System Log
  if(missing(logname))
  {
    logname <- "System"
  }
  
  ## Export System event log
  username <- shell("whoami",intern=T)
  username <- gsub(".+\\\\", "",username)
  computername <- shell("hostname",intern=T)
  
  filenamestring <- paste(username,computername, logname ,sep="_")
  filenamestring <- paste0(filenamestring, ".evtx")
  #cat(filenamestring)
  
  fileoutput <- paste0(datafiles_path,filenamestring)
  cmdstring <- paste0("wevtutil epl ", logname," \"", fileoutput, "\"")
  cat('\n',cmdstring, '\n', sep="")
  
  shell(cmdstring)
  
  cat('\n','File Output in: ',fileoutput, '\n', sep="")
  
  cat("\n######## END SCRIPT ########\n")
}

## TO get user input in cmd
# cat("Whats your name: ")
# x <- readLines(file("stdin"),1)
# print(x)

eventextract("file")