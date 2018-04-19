#### Service Desk Analysis ####

## This is analysis to measure the accuracy of service desk assignment
#### Logic #####
## 1a. filter vdi in related.ci.item -> filter out cell that not contain vdi in summary
## 1b. pick out the unique case number
## 2a. filter vdi in summary -> filter out cell that not contain vdi or virtualdesktop in relatedCIitem
## 2b. pick out the unique case number
## 3. combine the unique case number vector and select only unique number
## 4. tabulate result, create into graph, output into CSV

#### Initialize ####
rm(list=ls())
setwd("\\\\SVRSG001RPS01.asia.corp.anz.com\\pradana1$\\My Documents\\SGWS-8467837A_notbackedup")
getwd()


####################


######## Read Data ########
## xlsx
library(openxlsx)

filename <- "Top 10 CI and IA (Nov 06 - Nov 12, 2017).xlsx"
filepath <- paste0("datafiles/", filename)

df_full <- read.xlsx(filepath)

df <- df_full
df <- df[1:100,]

## Reset dataset to original set
df <- df_full

##########################

######## Clean Data ########

## Drop some columns
drops <- c("Description")
df <- df[,!(names(df) %in% drops)]
rm(drops)

## segregate name and LAN ID
reportedbylist <- strsplit(df$Reported.By, split='\\(')
df$Reported.By <- trimws(sapply(reportedbylist, '[[', 1))
df$ReportedByLanID <- do.call(rbind,reportedbylist)[,2]
df$ReportedByLanID <- gsub(')', '', df$ReportedByLanID)

assigneelist <- strsplit(df$Assignee, split='\\(')
df$Assignee <- trimws(do.call(rbind,assigneelist)[,1])
df$AssigneeLanID <- do.call(rbind,assigneelist)[,2]
df$AssigneeLanID <- gsub(')', '', df$AssigneeLanID)


rm(reportedbylist, assigneelist)
############################


######## Process Data ########

## Subset Only VDI in 'Related.-.CI.Name' column

df_vdi <- subset(df, grepl('vdi', df$`Related.-.CI.Name`, ignore.case = T))
df_vdi$correct <- ifelse(grepl('vdi', df_vdi$Summary, ignore.case = T), 1, 0)
df_vdi$correct <- ifelse(grepl('frozen', df_vdi$Summary, ignore.case = T), 1, df_vdi$correct)
uniqueCaseNo1 <- df_vdi[df_vdi$correct==0,]$Number

df_vdi2 <- subset(df, grepl('vdi', df$Summary, ignore.case = T))
df_vdi2$correct <- ifelse(grepl('vdi', df_vdi2$`Related.-.CI.Name`, ignore.case = T), 1, 0)
df_vdi2$correct <- ifelse(grepl('Virtual Desktop', df_vdi2$`Related.-.CI.Name`, ignore.case = T), 1, df_vdi$correct)
uniqueCaseNo2 <- df_vdi2[df_vdi2$correct==0,]$Number


#############################


######## Tabulate Result ########

## Combine the case number for all incorrect classification
combinedCaseNo <- c(uniqueCaseNo1, uniqueCaseNo2)

## subset all incorrect df for output
df_incorrect <- subset(df, df$Number %in% combinedCaseNo)

sort(table(df_incorrect$Reported.By),decreasing = T)[1:10]
sort(table(df_incorrect$Assignee),decreasing = T)[1:10]


## Plot Result
library(ggplot2)

## Top 10 Reported.By
reportedby_df <- as.data.frame(sort(table(df_incorrect$Reported.By),decreasing = T))
reportedby_df <- reportedby_df[1:10,]
names(reportedby_df) <- c("ReportedBy", "Frequency")

ggplot(data=reportedby_df, aes(x=reorder(ReportedBy, Frequency), y=Frequency, fill=ReportedBy)) + 
  geom_bar(stat='identity') +
  coord_flip() +
  ggtitle("Reported By MisClassification") + labs(x="Frequency", y="Reported By") +
  geom_text(aes(label=Frequency), hjust=-0.5)

## Percentage correct vs wrong classification
total_vdi_case <- nrow(df_vdi) + nrow(df_vdi2)
correct <- length(combinedCaseNo)
wrong <- total_vdi_case - correct
classification <- data.frame(label=c("wrong",'correct'), 
                             value=c(correct, wrong),
                             percentage=c(correct/total_vdi_case*100, wrong/total_vdi_case*100)
                             )
classification$percentage <- format(classification$percentage, digits=4)
rm(total_vdi_case,correct,wrong)
ggplot(data=classification, aes(x=label, y=percentage, fill=label)) +
  geom_bar(stat='identity') + 
  ggtitle("Accuracy of Classification") + labs(x="Classification", y="Percentage") +
  geom_text(aes(label=paste(percentage,'%')), vjust=-0.5)
  


#################################


######## Output Result ########

## Select column to write
selected <- c("Number", "Reported.By", "ReportedByLanID", "Assignee", "AssigneeLanID", "Summary", 
              "Related.-.CI.Name", "correct", "Reported.By.Group")
df_incorrect_result <- df_incorrect[, (names(df_incorrect) %in% selected)]
rm(selected)

output_filename <- gsub("\\.xlsx", "_result.csv", filepath)
write.csv(x = df_incorrect_result, file = output_filename)
rm(df_incorrect_result)


#################################


####### Processing Data #########
df1sub <- df_all[c("Number","Summary", "Related.-.CI.Name", "Status", "Incident.Area",
                "Description", "Affected.End.User", "Affected.End.User.Country")]
df1sub$Actions <- df1sub$Description
df1sub$Actions <- gsub(".+Actions Taken", "Actions Taken", df1sub$Actions, ignore.case = T)
df1sub$Actions <- gsub(".+Action Taken", "Action Taken", df1sub$Actions, ignore.case = T)
df1sub$Actions <- gsub("Issue Resolved.+", "Issue Resolved", df1sub$Actions, ignore.case = T)

df1sub$Actions <- gsub(".+Steps taken to resolve", "Steps taken to resolve", df1sub$Actions, ignore.case = T)
df1sub$Actions <- gsub("Did you know you can log.+", "Did you know you can log", df1sub$Actions, ignore.case = T)

write.csv(df1sub, file="datafiles/df1_processed.csv", row.names = FALSE)
################################  


