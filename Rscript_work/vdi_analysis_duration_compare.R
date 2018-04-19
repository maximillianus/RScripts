###################################################
########  VDI Analysis Duration Comparison ########
###################################################

rm(list=ls())
setwd("C:/NotBackedUp/")
source('Rscript/sqlconn_deepdive.R')
df <- sqlconn_dd('2017-12-08', '2017-12-31')
require(dplyr)
require(ggplot2)


#### Data Engineering ####

## Adding MW & TM
df$Branch <- "MW"
df$Branch <- ifelse(grepl("AU TM", df$Name), "TM", df$Branch)


## logon duration
df_logonduration <- subset(df, logonduration > 0)
result_logonduration <- df_logonduration %>% group_by(Branch, Date) %>% summarize(meanlogon=mean(logonduration)) %>% ungroup()
result_logonduration$Date <- as.factor(result_logonduration$Date)

# line chart
ggplot(data=result_logonduration, aes(x=Date,y=meanlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("Logon Duration MW vs TM")

## sd logon duration
sd_logonduration <- df_logonduration %>% group_by(Branch, Date) %>% summarize(sdlogon=sd(logonduration)) %>% ungroup()
sd_logonduration$Date <- as.factor(sd_logonduration$Date)

ggplot(data=sd_logonduration, aes(x=Date,y=sdlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("SD Logon Duration MW vs TM")



## HDXTimeMS
df_hdx <- subset(df, HDxTimeMS > 0)
result_hdx <- df_hdx %>% group_by(Branch, Date) %>% summarize(meanlogon=mean(HDxTimeMS)) %>% ungroup()
result_hdx$Date <- as.factor(result_hdx$Date)
  

# line chart
ggplot(data=result_hdx, aes(x=Date,y=meanlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("HDXTime Duration MW vs TM") + ylab("HDXTime(ms)")

## sd hdx time
sd_hdx <- df_hdx %>% group_by(Branch, Date) %>% summarize(sdlogon=sd(HDxTimeMS)) %>% ungroup()
sd_hdx$Date <- as.factor(sd_hdx$Date)

ggplot(data=sd_hdx, aes(x=Date,y=sdlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("SD HDX Time MW vs TM")

  
## Authentication Duration MS
df_auth <- subset(df, df$AuthenticationDurationMS > 0)
result_auth <- df_hdx %>% group_by(Branch, Date) %>% summarize(meanlogon=mean(AuthenticationDurationMS)) %>% ungroup()
result_auth$Date <- as.factor(result_auth$Date)


# line chart
ggplot(data=result_auth, aes(x=Date,y=meanlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("Authentication Duration MW vs TM") + ylab("AuthenticationDuration(ms)")

## sd auth time
sd_auth <- df_auth %>% group_by(Branch, Date) %>% summarize(sdlogon=sd(AuthenticationDurationMS)) %>% ungroup()
sd_auth$Date <- as.factor(sd_auth$Date)

ggplot(data=sd_auth, aes(x=Date,y=sdlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("SD Authentication Time MW vs TM")

## GpoTimeMS
df_gpo <- subset(df, df$GpoTimeMS > 0)
result_gpo <- df_hdx %>% group_by(Branch, Date) %>% summarize(meanlogon=mean(GpoTimeMS)) %>% ungroup()
result_gpo$Date <- as.factor(result_gpo$Date)


# line chart
ggplot(data=result_gpo, aes(x=Date,y=meanlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("Gpo Time MW vs TM") + ylab("GpoTime(ms)")

## sd GPO time
sd_gpo <- df_gpo %>% group_by(Branch, Date) %>% summarize(sdlogon=sd(GpoTimeMS)) %>% ungroup()
sd_gpo$Date <- as.factor(sd_gpo$Date)

ggplot(data=sd_gpo, aes(x=Date,y=sdlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("SD Gpo Time MW vs TM")



## Profile Load Time MS
df_profile <- subset(df, df$ProfileLoadTimeMS > 0)
result_profile <- df_hdx %>% group_by(Branch, Date) %>% summarize(meanlogon=mean(ProfileLoadTimeMS)) %>% ungroup()
result_profile$Date <- as.factor(result_profile$Date)


# line chart
ggplot(data=result_profile, aes(x=Date,y=meanlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("Profile Load Time MW vs TM") + ylab("Profile Load Time(ms)")

## sd Profileload time
sd_profile <- df_profile %>% group_by(Branch, Date) %>% summarize(sdlogon=sd(ProfileLoadTimeMS)) %>% ungroup()
sd_profile$Date <- as.factor(sd_profile$Date)

ggplot(data=sd_profile, aes(x=Date,y=sdlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("SD Profile Load Time MW vs TM")


## Interactive Time MS
df_interactive <- subset(df, df$InteractiveTimeMS > 0)
result_interactive <- df_hdx %>% group_by(Branch, Date) %>% summarize(meanlogon=mean(InteractiveTimeMS)) %>% ungroup()
result_interactive$Date <- as.factor(result_interactive$Date)


# line chart
ggplot(data=result_interactive, aes(x=Date,y=meanlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("Interactive Time MW vs TM") + ylab("Interactive Time(ms)")

## sd Interactive time
sd_interactive <- df_interactive %>% group_by(Branch, Date) %>% summarize(sdlogon=sd(InteractiveTimeMS)) %>% ungroup()
sd_interactive$Date <- as.factor(sd_interactive$Date)

ggplot(data=sd_interactive, aes(x=Date,y=sdlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("SD Interactive Time MW vs TM")


## LogonScriptTime MS
df_logonscript <- subset(df, df$LogonScriptTimeMS > 0)
result_logonscript <- df_hdx %>% group_by(Branch, Date) %>% summarize(meanlogon=mean(LogonScriptTimeMS)) %>% ungroup()
result_logonscript$Date <- as.factor(result_logonscript$Date)


# line chart
ggplot(data=result_logonscript, aes(x=Date,y=meanlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("Logon Script Time MW vs TM") + ylab("Logon Script Time(ms)")

## sd Logon Script time
sd_logonscript <- df_logonscript %>% group_by(Branch, Date) %>% summarize(sdlogon=sd(LogonScriptTimeMS)) %>% ungroup()
sd_logonscript$Date <- as.factor(sd_logonscript$Date)

ggplot(data=sd_logonscript, aes(x=Date,y=sdlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("SD Logon Script Time MW vs TM")


## Brokering Duration MS
df_broker <- subset(df, df$BrokeringDurationMS > 0)
result_broker <- df_hdx %>% group_by(Branch, Date) %>% summarize(meanlogon=mean(BrokeringDurationMS)) %>% ungroup()
result_broker$Date <- as.factor(result_broker$Date)


# line chart
ggplot(data=result_broker, aes(x=Date,y=meanlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("Brokering Duration Time MW vs TM") + ylab("Brokering Duration Time(ms)")

## sd Interactive time
sd_broker <- df_broker %>% group_by(Branch, Date) %>% summarize(sdlogon=sd(BrokeringDurationMS)) %>% ungroup()
sd_broker$Date <- as.factor(sd_broker$Date)

ggplot(data=sd_broker, aes(x=Date,y=sdlogon,group=Branch, colour=Branch)) + 
  geom_line() + geom_vline(xintercept=9) +
  geom_point() +
  theme(axis.text.x = element_text(angle=30, vjust=0.5) ) + 
  ggtitle("SD Brokering Duration Time MW vs TM")


