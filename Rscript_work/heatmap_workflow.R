######## HEATMAP WORKFLOW for 2 WEEKS ########
## SQL connection to get data for 2 weeks
source('Rscript/sqlconn.R')
df <- sqlconn('2017-07-14')
## Calculate Data for 2 weeks
source('Rscript/calcdifflogon.R')
calcdifflogon(df,'week','mean')
#calcdifflogon(df,'week','median')
#calcdifflogon(df,'week','sd')
##############################################

#### NOTE ####
#
# Please ignore calcdifflogon(df, 'week' ,'median') and 
# calcdifflogon(df, 'week', 'sd'). Originally it is used to calculate statistics
# for both median and std dev individually but latest revision of the code will
# calculate all 3 stats under calcdifflogon(df, 'week', 'mean')
#
##############