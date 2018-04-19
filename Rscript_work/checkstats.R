checkstats <- function(df)
{
	##This function queries from SQL all the machines and instances from Mar-Apr which AvgValue > 880	

	##Initialize
	cty <- unique(df$Country)
	mc <- unique(df$MachineType)
	mcname <- unique(df$MachineName)
	inst <- unique(df$InstanceName)

	instlist <- paste0('\'',inst,'\'', collapse=",")
	mcnamelist <- 	paste0('\'',mcname,'\'', collapse=",")

	q <- paste0("SELECT Country,MachineName, InstanceName, AverageValue, Date FROM PerformanceWorkstationData
		 WHERE Date >= '2017-03-01 00:00:00.000' AND Date < '2017-04-19 00:00:00.000'
		 AND CounterName = 'Private Bytes'
		 AND MachineName IN (", mcnamelist ,") 
		 AND InstanceName IN (", instlist,") 
		 ")

	cat(q, '\n')
	if(exists("dat2") == F)
	{
	  sqlDS <- RxSqlServerData(connectionString=sqlServerConn, sqlQuery=q)
	  dat2 <<- rxImport(sqlDS, rowsPerRead=30000, reportProgress=2)
	} else {
	  cat('Variable dat2 exists. Query skipped. \n')
	}
	
	#new <- tapply(dat2$AverageValue, list(
	##Loop and Print
	for(i in cty)
	{
	  #cat(i, '\n')
		
		for(j in mc)
		{
		  #cat('\t',j,'\n')
		  for(h in inst)
		  {
		    #cat('\t  ',h,'\n')
		    df.submc <- subset(df, Country==i & MachineType==j)
		    mcname <- unique(df.submc$MachineName)
		    for(k in mcname)
		    {
			#cat('\t\t', k, '\n')
		    }
		  }
		}
	}


	#Check if Code reaches END
	cat("End Script\n")
}