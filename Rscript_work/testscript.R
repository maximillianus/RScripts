
##Installed
#serverdir <-  "\\\\SVRSG001RPS01.asia.corp.anz.com\\pradana1$\\My Documents\\SGWS-8467837A_notbackedup\\Rscript"

## This below is script to get the directory where this testscript.R is being run
## It will work using Rstudio's source() and CMD Rscript.exe

# thisFile <- function() {
#   cmdArgs <- commandArgs(trailingOnly = FALSE)
#   needle <- "--file="
#   match <- grep(needle, cmdArgs)
#   if (length(match) > 0) {
#     # Rscript
#     return(normalizePath(sub(needle, "", cmdArgs[match])))
#   } else {
#     # 'source'd via R console
#     return(normalizePath(sys.frames()[[1]]$ofile))
#   }
# }
# 
# script.dir <- dirname(thisFile())
# setwd(script.dir)
# print(getwd())

####################################################################

## Script to prompt for user input

# cat("Whats your name: ")
# x <- readLines(file("stdin"),1)
# print(x)

  